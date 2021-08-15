/*
** $Id: lcode.c,v 2.62.1.1 2013/04/12 18:48:47 roberto Exp $
** Code generator for Lua
** See Copyright Notice in lua.h
*/


#include <stdlib.h>

#define lcode_c
#define LUA_CORE

#include "lua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstring.h"
#include "ltable.h"
#include "lvm.h"


#define hasjumps(e) ((e)->t != (e)->f)


static int isnumeral(expdesc *e)
{
    return (e->k == VKNUM && e->t == NO_JUMP && e->f == NO_JUMP);
}

/* 编码加载nil
 * @param from 寄存器起始标号
 * @param n 设置nil的个数
 */
void luaK_nil (FuncState *fs, int from, int n)
{
    Instruction *previous;
    int l = from + n - 1;  /* last register to set nil */
    if (fs->pc > fs->lasttarget)    /* no jumps to current position? */
    {
        previous = &fs->f->code[fs->pc-1];
        if (GET_OPCODE(*previous) == OP_LOADNIL) /* 感觉在进行合并操作 */
        {
            int pfrom = GETARG_A(*previous); /* from */
            int pl = pfrom + GETARG_B(*previous); /* last */
            if ((pfrom <= from && from <= pl + 1) ||
                (from <= pfrom && pfrom <= l + 1))    /* can connect both? */
            {
                if (pfrom < from) from = pfrom;  /* from = min(from, pfrom) */
                if (pl > l) l = pl;  /* l = max(l, pl) */
                SETARG_A(*previous, from);
                SETARG_B(*previous, l - from);
                return;
            }
        }  /* else go through */
    }
    luaK_codeABC(fs, OP_LOADNIL, from, n - 1, 0);  /* else no optimization */
}

/* 生成jump指令
 * @return 返回生成的指令的偏移量
 */
int luaK_jump (FuncState *fs)
{
    /* 保存下jump */
    int jpc = fs->jpc;  /* save list of jumps to here */
    int j;
    fs->jpc = NO_JUMP;
    /* OP_JMP A sBx   pc+=sBx; if (A) close all upvalues >= R(A - 1)  */
    j = luaK_codeAsBx(fs, OP_JMP, 0, NO_JUMP); /* 获得jmp指令的相对偏移 */
    luaK_concat(fs, &j, jpc);  /* keep them on hold */
    return j;
}

/* 编码return指令
 * @param first 寄存器编号
 * @param nret 返回值个数
 */
void luaK_ret (FuncState *fs, int first, int nret)
{
    luaK_codeABC(fs, OP_RETURN, first, nret+1, 0);
}

/* 生成跳转命令
 * @return 生成的指令的偏移量
 */
static int condjump (FuncState *fs, OpCode op, int A, int B, int C)
{
    luaK_codeABC(fs, op, A, B, C); /* 每生成一条指令,会使得pc指针+1 */
    return luaK_jump(fs);
}

/* 修正jump指令
 * @param dest 待跳转的指令
 */
static void fixjump (FuncState *fs, int pc, int dest)
{
    Instruction *jmp = &fs->f->code[pc]; /* 获取到跳转指令 */
    int offset = dest-(pc+1); /* 计算相对偏移 */
    lua_assert(dest != NO_JUMP);
    if (abs(offset) > MAXARG_sBx)
        luaX_syntaxerror(fs->ls, "control structure too long");
    SETARG_sBx(*jmp, offset); /* 重新设置偏移量 */
}


/*
** returns current `pc' and marks it as a jump target (to avoid wrong
** optimizations with consecutive instructions not in the same basic block).
*/
/* 返回当前pc的值,并将其标记为一个jump target */
int luaK_getlabel (FuncState *fs)
{
    fs->lasttarget = fs->pc;
    return fs->pc;
}


static int getjump (FuncState *fs, int pc)
{
    int offset = GETARG_sBx(fs->f->code[pc]);
    if (offset == NO_JUMP)  /* point to itself represents end of list */
        return NO_JUMP;  /* end of list */
    else
        return (pc+1)+offset;  /* turn offset into absolute position */
}


static Instruction *getjumpcontrol (FuncState *fs, int pc)
{
    Instruction *pi = &fs->f->code[pc];
    if (pc >= 1 && testTMode(GET_OPCODE(*(pi-1))))
        return pi-1;
    else
        return pi;
}


/*
** check whether list has any jump that do not produce a value
** (or produce an inverted value)
*/
static int need_value (FuncState *fs, int list)
{
    for (; list != NO_JUMP; list = getjump(fs, list))
    {
        Instruction i = *getjumpcontrol(fs, list); /* 获得指令 */
        if (GET_OPCODE(i) != OP_TESTSET) return 1;
    }
    return 0;  /* not found */
}

/* 修正test指令生成的代码
 *
 */
static int patchtestreg (FuncState *fs, int node, int reg)
{
    Instruction *i = getjumpcontrol(fs, node);
    if (GET_OPCODE(*i) != OP_TESTSET)
        return 0;  /* cannot patch other instructions */
    /* TESTSET A B C  if (boolean(R(B)) != C) then PC++ else R(A) := R(B)
     * TEST   A  C  if (boolean(R(A)) != C) then PC++
     */
    if (reg != NO_REG && reg != GETARG_B(*i))
        SETARG_A(*i, reg);
    else  /* no register to put value or register already has the value */
        /* 转换为TEST指令 */
        *i = CREATE_ABC(OP_TEST, GETARG_B(*i), 0, GETARG_C(*i));

    return 1;
}


static void removevalues (FuncState *fs, int list)
{
    for (; list != NO_JUMP; list = getjump(fs, list))
        patchtestreg(fs, list, NO_REG);
}

/* 修正生成的jump指令
 * @param list 指令偏移
 * @param dtarget 需要jump的目的地
 * @param vtarget
 */
static void patchlistaux (FuncState *fs, int list, int vtarget, int reg,
                          int dtarget)
{
    while (list != NO_JUMP)
    {
        int next = getjump(fs, list);
        if (patchtestreg(fs, list, reg)) /* 这里将每一条指令的跳转参数都修改为了同一个值 */
            fixjump(fs, list, vtarget);
        else
            fixjump(fs, list, dtarget);  /* jump to default target */
        list = next;
    }
}


static void dischargejpc (FuncState *fs)
{
    patchlistaux(fs, fs->jpc, fs->pc, NO_REG, fs->pc);
    fs->jpc = NO_JUMP;
}

/* 将跳转指令的目的地都设置为target
 * @param target 跳转目的地
 * @param list 待修正的跳转指令构成的链表
 */
void luaK_patchlist (FuncState *fs, int list, int target)
{
    if (target == fs->pc)
        luaK_patchtohere(fs, list);
    else
    {
        lua_assert(target < fs->pc);
        patchlistaux(fs, list, target, NO_REG, target);
    }
}


/* 修正跳转指令
 * @param list 待修正的指令构建成的链表
 * @param level 跳转位置
 */
LUAI_FUNC void luaK_patchclose (FuncState *fs, int list, int level)
{
    level++;  /* argument is +1 to reserve 0 as non-op */
    while (list != NO_JUMP)
    {
        int next = getjump(fs, list);
        lua_assert(GET_OPCODE(fs->f->code[list]) == OP_JMP &&
                   (GETARG_A(fs->f->code[list]) == 0 ||
                    GETARG_A(fs->f->code[list]) >= level));
        SETARG_A(fs->f->code[list], level);
        list = next;
    }
}

/* 将list放入jpc链表,表示list上所有的指令都要jump到下一条指令
 * 修正操作在dischargejpc中进行
 */
void luaK_patchtohere (FuncState *fs, int list)
{
    luaK_getlabel(fs); /* 将要跳转的指令记录下来 */
    luaK_concat(fs, &fs->jpc, list);
}

/* 将要修正的指令加入链表,链表的节点是一个指令偏移
 * 这里复用了JMP指令的sBx参数
 * @param l2 指令偏移
 * @param l1 待修正指令构成的链表
 */
void luaK_concat (FuncState *fs, int *l1, int l2)
{
     /*  OP_JMP A sBx   pc+=sBx; if (A) close all upvalues >= R(A - 1)  */
    if (l2 == NO_JUMP) return;
    else if (*l1 == NO_JUMP)
        *l1 = l2; /* 记录下来 */
    else
    {
        int list = *l1; /* 这里面实际保存了待重定位的jump指令的偏移 */
        int next;
        /* 这里会一直找,直到找到最后一个节点 */
        while ((next = getjump(fs, list)) != NO_JUMP)  /* find last element */
            list = next;
        /* 将l2设置到JMP指令的sBx中去 */
        fixjump(fs, list, l2);
    }
}

/* 特别要注意这个函数,这里会将生成的指令保存起来
 */
static int luaK_code (FuncState *fs, Instruction i)
{
    Proto *f = fs->f;
    /* 用当前的指令位置,修正处于pending状态的jump指令 */
    dischargejpc(fs);  /* `pc' will change */
    /* put new instruction in code array */
    luaM_growvector(fs->ls->L, f->code, fs->pc, f->sizecode, Instruction,
                    MAX_INT, "opcodes");
    f->code[fs->pc] = i; /* 记录下对应的指令 */
    /* save corresponding line information */
    luaM_growvector(fs->ls->L, f->lineinfo, fs->pc, f->sizelineinfo, int,
                    MAX_INT, "opcodes");
    f->lineinfo[fs->pc] = fs->ls->lastline; /* 记录下对应的行 */
    return fs->pc++;
}


int luaK_codeABC (FuncState *fs, OpCode o, int a, int b, int c)
{
    lua_assert(getOpMode(o) == iABC);
    lua_assert(getBMode(o) != OpArgN || b == 0);
    lua_assert(getCMode(o) != OpArgN || c == 0);
    lua_assert(a <= MAXARG_A && b <= MAXARG_B && c <= MAXARG_C);
    return luaK_code(fs, CREATE_ABC(o, a, b, c));
}


int luaK_codeABx (FuncState *fs, OpCode o, int a, unsigned int bc)
{
    lua_assert(getOpMode(o) == iABx || getOpMode(o) == iAsBx);
    lua_assert(getCMode(o) == OpArgN);
    lua_assert(a <= MAXARG_A && bc <= MAXARG_Bx);
    return luaK_code(fs, CREATE_ABx(o, a, bc));
}


static int codeextraarg (FuncState *fs, int a)
{
    lua_assert(a <= MAXARG_Ax);
    return luaK_code(fs, CREATE_Ax(OP_EXTRAARG, a));
}

/* 加载常量至reg对应的寄存器中
 * @param reg 寄存器编号
 */
int luaK_codek (FuncState *fs, int reg, int k)
{
    if (k <= MAXARG_Bx) /* 将常量加载到寄存器中 */
        return luaK_codeABx(fs, OP_LOADK, reg, k);
    else
    {
        int p = luaK_codeABx(fs, OP_LOADKX, reg, 0);
        codeextraarg(fs, k);
        return p;
    }
}


void luaK_checkstack (FuncState *fs, int n)
{
    int newstack = fs->freereg + n;
    if (newstack > fs->f->maxstacksize)
    {
        if (newstack >= MAXSTACK)
            luaX_syntaxerror(fs->ls, "function or expression too complex");
        fs->f->maxstacksize = cast_byte(newstack);
    }
}

/* 调整寄存器标号,预先分配n个寄存器
 * @param n 预分配的寄存器的个数
 */
void luaK_reserveregs (FuncState *fs, int n)
{
    luaK_checkstack(fs, n);
    fs->freereg += n;
}


static void freereg (FuncState *fs, int reg)
{
    if (!ISK(reg) && reg >= fs->nactvar)
    {
        fs->freereg--;
        lua_assert(reg == fs->freereg);
    }
}

/* 将表达式的值从寄存器中移除,前提是表达式的值已经位于寄存器中
 *
 */
static void freeexp (FuncState *fs, expdesc *e)
{
    if (e->k == VNONRELOC)
        freereg(fs, e->u.info);
}

/* 添加至常量表中,返回在常量表中的索引值
 * @param key, v 待添加值常量表中的键值
 */
static int addk (FuncState *fs, TValue *key, TValue *v)
{
    lua_State *L = fs->ls->L;
    TValue *idx = luaH_set(L, fs->h, key);
    Proto *f = fs->f;
    int k, oldsize;
    if (ttisnumber(idx))
    {
        lua_Number n = nvalue(idx);
        lua_number2int(k, n);
        if (luaV_rawequalobj(&f->k[k], v))
            return k;
        /* else may be a collision (e.g., between 0.0 and "\0\0\0\0\0\0\0\0");
           go through and create a new entry for this value */
    }
    /* constant not found; create a new entry */
    oldsize = f->sizek;
    k = fs->nk;
    /* numerical value does not need GC barrier;
       table has no metatable, so it does not need to invalidate cache */
    setnvalue(idx, cast_num(k));
    luaM_growvector(L, f->k, k, f->sizek, TValue, MAXARG_Ax, "constants");
    while (oldsize < f->sizek) setnilvalue(&f->k[oldsize++]);
    setobj(L, &f->k[k], v);
    fs->nk++;
    luaC_barrier(L, f, v);
    return k;
}

/* 编码字符常量
 * @param s 待编码的字符串常量
 * @return 字符在常量表中的索引值
 */
int luaK_stringK (FuncState *fs, TString *s)
{
    TValue o;
    setsvalue(fs->ls->L, &o, s);
    return addk(fs, &o, &o);
}

/* 编码数字常量
 * @param r 待编码的数字常量
 * @return 数字在常量表中的索引值
 */
int luaK_numberK (FuncState *fs, lua_Number r)
{
    int n;
    lua_State *L = fs->ls->L;
    TValue o;
    setnvalue(&o, r);
    if (r == 0 || luai_numisnan(NULL, r))    /* handle -0 and NaN */
    {
        /* use raw representation as key to avoid numeric problems */
        setsvalue(L, L->top++, luaS_newlstr(L, (char *)&r, sizeof(r)));
        n = addk(fs, L->top - 1, &o);
        L->top--;
    }
    else
        n = addk(fs, &o, &o);  /* regular case */
    return n;
}

/* 编码bool常量
 * @param b 待编码的bool值
 * @return bool值在常量表中的索引值
 */
static int boolK (FuncState *fs, int b)
{
    TValue o;
    setbvalue(&o, b);
    return addk(fs, &o, &o);
}


static int nilK (FuncState *fs)
{
    TValue k, v;
    setnilvalue(&v);
    /* cannot use nil as key; instead use table itself to represent nil */
    sethvalue(fs->ls->L, &k, fs->h);
    return addk(fs, &k, &v);
}

/* 设置返回值个数
 * @param nresults 返回值个数
 */
void luaK_setreturns (FuncState *fs, expdesc *e, int nresults)
{
    if (e->k == VCALL)    /* expression is an open function call? */
    { /* y(1,2,3) */
        /* OP_CALL A B C  R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
         * A 函数, B+1 参数个数 C+1 返回值个数
         */
        SETARG_C(getcode(fs, e), nresults+1);
    }
    else if (e->k == VVARARG)
    {
        /* OP_VARARG  A B R(A), R(A+1), ..., R(A+B-1) = vararg
         * 从可变参数中加载B-1个到以编号A开始的寄存器中
         */
        SETARG_B(getcode(fs, e), nresults+1); /* nresults个值 */
        SETARG_A(getcode(fs, e), fs->freereg); /* 起始寄存器 */
        luaK_reserveregs(fs, 1);
    }
}

/* 仅有一个返回值
 *
 */
void luaK_setoneret (FuncState *fs, expdesc *e)
{
    /* 表达式的值要通过函数获得 */
    if (e->k == VCALL)    /* expression is an open function call? */
    {
        e->k = VNONRELOC;
        e->u.info = GETARG_A(getcode(fs, e));
    }
    else if (e->k == VVARARG) /* 通过可变参数获取值 */
    {
        SETARG_B(getcode(fs, e), 2);
        e->k = VRELOCABLE;  /* can relocate its simple result */
    }
}

/* 解引用变量值
 * 也就是编码,如何获取到表达式e的值
 */
void luaK_dischargevars (FuncState *fs, expdesc *e)
{
    switch (e->k)
    {
        case VLOCAL: /* 局部变量已经在寄存器中 */
        {
            e->k = VNONRELOC;
            break;
        }
        case VUPVAL: /* 表达式的值为upvalue */
        {
            /* 将upvalue的值加载至寄存器a中,这里为0,需要后续替换 */
            e->u.info = luaK_codeABC(fs, OP_GETUPVAL, 0, e->u.info, 0); /* 通过upvalue来加载e的值 */
            e->k = VRELOCABLE; /* 表达式的结果需要加载至寄存器 */
            break;
        }
        case VINDEXED: /* 需要通过表来访问值 */
        {
            OpCode op = OP_GETTABUP;  /* assume 't' is in an upvalue */
            freereg(fs, e->u.ind.idx);
            if (e->u.ind.vt == VLOCAL)    /* 't' is in a register? */
            {
                freereg(fs, e->u.ind.t);
                op = OP_GETTABLE; /* 从表中加载 */
            }
            e->u.info = luaK_codeABC(fs, op, 0, e->u.ind.t, e->u.ind.idx);
            e->k = VRELOCABLE; /* 结果需要加载至寄存器 */
            break;
        }
        case VVARARG:
        case VCALL:
        {
            luaK_setoneret(fs, e);
            break;
        }
        default:
            break;  /* there is one value available (somewhere) */
    }
}


static int code_label (FuncState *fs, int A, int b, int jump)
{
    luaK_getlabel(fs);  /* those instructions may be jump targets */
    /* OP_LOADBOOL  A B C   R(A) := (Bool)B; if (C) pc++   */
    return luaK_codeABC(fs, OP_LOADBOOL, A, b, jump);
}

/* 将表达式e安置到寄存器(标号为reg)之中
 * 如果e已经位于寄存器之中,但是寄存器并非reg,需要执行move操作
 */
static void discharge2reg (FuncState *fs, expdesc *e, int reg)
{
    luaK_dischargevars(fs, e);
    switch (e->k)
    {
        case VNIL:
        {
            /* 加载nil */
            luaK_nil(fs, reg, 1);
            break;
        }
        case VFALSE:
        case VTRUE:
        {
            /* 加载bool变量 */
            luaK_codeABC(fs, OP_LOADBOOL, reg, e->k == VTRUE, 0);
            break;
        }
        case VK:
        {
            /* 加载常量 */
            luaK_codek(fs, reg, e->u.info);
            break;
        }
        case VKNUM:
        {
            luaK_codek(fs, reg, luaK_numberK(fs, e->u.nval));
            break;
        }
        case VRELOCABLE:
        {
            Instruction *pc = &getcode(fs, e);
            SETARG_A(*pc, reg);
            break;
        }
        case VNONRELOC:
        {
            if (reg != e->u.info) /* e在另外一个寄存器之中,执行move操作 */
                luaK_codeABC(fs, OP_MOVE, reg, e->u.info, 0);
            break;
        }
        default:
        {
            lua_assert(e->k == VVOID || e->k == VJMP);
            return;  /* nothing to do... */
        }
    }
    e->u.info = reg;
    e->k = VNONRELOC;
}


static void discharge2anyreg (FuncState *fs, expdesc *e)
{
    if (e->k != VNONRELOC)
    {
        luaK_reserveregs(fs, 1);
        discharge2reg(fs, e, fs->freereg-1);
    }
}

/* 将表达式的值放入寄存器
 * @param e 表达式描述
 * @param reg 新寄存器标号
 */
static void exp2reg (FuncState *fs, expdesc *e, int reg)
{
    discharge2reg(fs, e, reg);
    if (e->k == VJMP)
        luaK_concat(fs, &e->t, e->u.info);  /* put this jump in `t' list */
    if (hasjumps(e))
    {
        int final;  /* position after whole expression */
        int p_f = NO_JUMP;  /* position of an eventual LOAD false */
        int p_t = NO_JUMP;  /* position of an eventual LOAD true */
        if (need_value(fs, e->t) || need_value(fs, e->f))
        {
            int fj = (e->k == VJMP) ? NO_JUMP : luaK_jump(fs);
            p_f = code_label(fs, reg, 0, 1);
            p_t = code_label(fs, reg, 1, 0);
            luaK_patchtohere(fs, fj);
        }
        final = luaK_getlabel(fs);
        patchlistaux(fs, e->f, final, reg, p_f);
        patchlistaux(fs, e->t, final, reg, p_t);
    }
    e->f = e->t = NO_JUMP;
    e->u.info = reg;
    e->k = VNONRELOC; /* 已经加载至寄存器 */
}

/* 将表达式计算出的结果加载至下一个寄存器
 * @param e 表达式
 */
void luaK_exp2nextreg (FuncState *fs, expdesc *e)
{
    /* 计算exp的值 */
    luaK_dischargevars(fs, e);
    freeexp(fs, e);
    /* 如果表达式计算出多个值,怎么办? */
    luaK_reserveregs(fs, 1); /* 保留一个寄存器 */
    /* 将exp计算后的值加载至对应的寄存器 */
    exp2reg(fs, e, fs->freereg - 1);
}

/* 将表达式加载到寄存器中
 *
 */
int luaK_exp2anyreg (FuncState *fs, expdesc *e)
{
    luaK_dischargevars(fs, e);
    if (e->k == VNONRELOC)
    {
        if (!hasjumps(e)) return e->u.info;  /* exp is already in a register */
        if (e->u.info >= fs->nactvar)    /* reg. is not a local? */
        {
            exp2reg(fs, e, e->u.info);  /* put value on it */
            return e->u.info;
        }
    }
    luaK_exp2nextreg(fs, e);  /* default */
    return e->u.info;
}


void luaK_exp2anyregup (FuncState *fs, expdesc *e)
{
    if (e->k != VUPVAL || hasjumps(e))
        luaK_exp2anyreg(fs, e);
}


void luaK_exp2val (FuncState *fs, expdesc *e)
{
    if (hasjumps(e))
        luaK_exp2anyreg(fs, e);
    else
        luaK_dischargevars(fs, e);
}

/* 将表达式的值加载至寄存器或者常量表中
 *
 */
int luaK_exp2RK (FuncState *fs, expdesc *e)
{
    luaK_exp2val(fs, e);
    switch (e->k)
    {
        case VTRUE:
        case VFALSE:
        case VNIL:
        {
            if (fs->nk <= MAXINDEXRK)    /* constant fits in RK operand? */
            {
                e->u.info = (e->k == VNIL) ? nilK(fs) : boolK(fs, (e->k == VTRUE));
                e->k = VK;
                return RKASK(e->u.info);
            }
            else break;
        }
        case VKNUM:
        {
            e->u.info = luaK_numberK(fs, e->u.nval);
            e->k = VK;
            /* go through */
        }
        case VK:
        {
            if (e->u.info <= MAXINDEXRK)  /* constant fits in argC? */
                return RKASK(e->u.info);
            else break;
        }
        default:
            break;
    }
    /* not a constant in the right range: put it in a register */
    return luaK_exp2anyreg(fs, e);
}

/* 将表达式的值存储起来
 *
 */
void luaK_storevar (FuncState *fs, expdesc *var, expdesc *ex)
{
    switch (var->k)
    {
        case VLOCAL: /* 局部变量 */
        {
            freeexp(fs, ex);
            exp2reg(fs, ex, var->u.info);
            return;
        }
        case VUPVAL: /* upvalue */
        {
            int e = luaK_exp2anyreg(fs, ex);
            /* 寄存器的值放入upvalue之中 */
            luaK_codeABC(fs, OP_SETUPVAL, e, var->u.info, 0);
            break;
        }
        case VINDEXED:
        {
            OpCode op = (var->u.ind.vt == VLOCAL) ? OP_SETTABLE : OP_SETTABUP;
            int e = luaK_exp2RK(fs, ex);
            luaK_codeABC(fs, op, var->u.ind.t, var->u.ind.idx, e);
            break;
        }
        default:
        {
            lua_assert(0);  /* invalid var kind to store */
            break;
        }
    }
    /* 很重要的一个步骤就是调整好寄存器,使得寄存器分配不过多,也不过少 */
    freeexp(fs, ex);
}


void luaK_self (FuncState *fs, expdesc *e, expdesc *key)
{
    int ereg;
    luaK_exp2anyreg(fs, e);
    ereg = e->u.info;  /* register where 'e' was placed */
    freeexp(fs, e);
    e->u.info = fs->freereg;  /* base register for op_self */
    e->k = VNONRELOC;
    luaK_reserveregs(fs, 2);  /* function and 'self' produced by op_self */
    luaK_codeABC(fs, OP_SELF, e->u.info, ereg, luaK_exp2RK(fs, key));
    freeexp(fs, key);
}


static void invertjump (FuncState *fs, expdesc *e)
{
    Instruction *pc = getjumpcontrol(fs, e->u.info);
    lua_assert(testTMode(GET_OPCODE(*pc)) && GET_OPCODE(*pc) != OP_TESTSET &&
               GET_OPCODE(*pc) != OP_TEST);
    /* jump指令
     * OP_JMP A sBx   pc+=sBx; if (A) close all upvalues >= R(A - 1)
     */
    SETARG_A(*pc, !(GETARG_A(*pc)));
}

/* 生成jump指令
 * @return 生成的jump指令的偏移量
 */
static int jumponcond (FuncState *fs, expdesc *e, int cond)
{
    if (e->k == VRELOCABLE) /* 需要加载至寄存器 */
    {
        Instruction ie = getcode(fs, e); /* 获得e对应的指令 */
        if (GET_OPCODE(ie) == OP_NOT)
        {
            fs->pc--;  /* remove previous OP_NOT */
            /*  OP_TEST A C if not (R(A) <=> C) then pc++  */
            return condjump(fs, OP_TEST, GETARG_B(ie), 0, !cond);
        }
        /* else go through */
    }
    discharge2anyreg(fs, e);
    freeexp(fs, e);
    /* OP_TESTSET A B C   if (R(B) <=> C) then R(A) := R(B) else pc++ */
    /* 这里的参数A是需要后面进行填充 */
    return condjump(fs, OP_TESTSET, NO_REG, e->u.info, cond);
}

/* 编码if语句true分支
 * 如果表达式结果为false,需要跳转
 * @param e condition 需要根据这个条件来判定是否需要跳转
 */
void luaK_goiftrue (FuncState *fs, expdesc *e)
{
    int pc;  /* pc of last jump */
    luaK_dischargevars(fs, e);
    switch (e->k)
    {
        case VJMP:
        {
            invertjump(fs, e);
            pc = e->u.info;
            break;
        }
        case VK:
        case VKNUM:
        case VTRUE:
        {
            pc = NO_JUMP;  /* always true; do nothing */
            break;
        }
        default: /* condition为false,需要跳转 */
        {
            /* if e != 0 jump */
            pc = jumponcond(fs, e, 0); /* 为false时才进行跳转 */
            break;
        }
    }
    /* false分支要执行pc指代的指令,后面parse的过程中,会将实际指令的偏移填充进去 */
    luaK_concat(fs, &e->f, pc);  /* insert last jump in `f' list */
    /* true分支,重新修正生成指令中jump指令的偏移 */
    luaK_patchtohere(fs, e->t);
    e->t = NO_JUMP;
}

/* 为false分支生成指令
 * @param e 表达式
 */
void luaK_goiffalse (FuncState *fs, expdesc *e)
{
    int pc;  /* pc of last jump */
    luaK_dischargevars(fs, e);
    switch (e->k)
    {
        case VJMP:
        {
            pc = e->u.info;
            break;
        }
        case VNIL:
        case VFALSE:
        {
            pc = NO_JUMP;  /* always false; do nothing */
            break;
        }
        default:
        {
            /* true分支执行pc指令 */
            pc = jumponcond(fs, e, 1);
            break;
        }
    }
    luaK_concat(fs, &e->t, pc);  /* insert last jump in `t' list */
    luaK_patchtohere(fs, e->f); /* false分支jump到下一条指令 */
    e->f = NO_JUMP;
}

/* 为not表达式生成指令
 * @param e 表达式
 */
static void codenot (FuncState *fs, expdesc *e)
{
    luaK_dischargevars(fs, e);
    switch (e->k)
    {
        case VNIL:
        case VFALSE:
        {
            e->k = VTRUE;
            break;
        }
        case VK:
        case VKNUM:
        case VTRUE:
        {
            e->k = VFALSE;
            break;
        }
        case VJMP:
        {
            invertjump(fs, e);
            break;
        }
        case VRELOCABLE:
        case VNONRELOC:
        {
            discharge2anyreg(fs, e); /* 将表达式的值加载到寄存器 */
            freeexp(fs, e);
            e->u.info = luaK_codeABC(fs, OP_NOT, 0, e->u.info, 0);
            e->k = VRELOCABLE;
            break;
        }
        default:
        {
            lua_assert(0);  /* cannot happen */
            break;
        }
    }
    /* interchange true and false lists */
    {
        int temp = e->f;
        e->f = e->t;
        e->t = temp;
    }
    removevalues(fs, e->f);
    removevalues(fs, e->t);
}


void luaK_indexed (FuncState *fs, expdesc *t, expdesc *k)
{
    lua_assert(!hasjumps(t));
    t->u.ind.t = t->u.info;
    t->u.ind.idx = luaK_exp2RK(fs, k);
    t->u.ind.vt = (t->k == VUPVAL) ? VUPVAL
                  : check_exp(vkisinreg(t->k), VLOCAL);
    t->k = VINDEXED; /* 需要通过表来访问 */
}

/* 直接将结果计算出来 */
static int constfolding (OpCode op, expdesc *e1, expdesc *e2)
{
    lua_Number r;
    if (!isnumeral(e1) || !isnumeral(e2)) return 0;
    if ((op == OP_DIV || op == OP_MOD) && e2->u.nval == 0)
        return 0;  /* do not attempt to divide by 0 */
    r = luaO_arith(op - OP_ADD + LUA_OPADD, e1->u.nval, e2->u.nval);
    e1->u.nval = r;
    return 1;
}


static void codearith (FuncState *fs, OpCode op,
                       expdesc *e1, expdesc *e2, int line)
{
    if (constfolding(op, e1, e2))
        return;
    else
    {
        int o2 = (op != OP_UNM && op != OP_LEN) ? luaK_exp2RK(fs, e2) : 0;
        int o1 = luaK_exp2RK(fs, e1);
        if (o1 > o2)
        {
            freeexp(fs, e1);
            freeexp(fs, e2);
        }
        else
        {
            freeexp(fs, e2);
            freeexp(fs, e1);
        }
        e1->u.info = luaK_codeABC(fs, op, 0, o1, o2); /* 执行编码操作 */
        e1->k = VRELOCABLE;
        luaK_fixline(fs, line);
    }
}

/* 编码
 * @param op 指令
 * @param cond 条件(1 or 0)
 * @param e1,e2 条件
 */
static void codecomp (FuncState *fs, OpCode op, int cond, expdesc *e1,
                      expdesc *e2)
{
    int o1 = luaK_exp2RK(fs, e1);
    int o2 = luaK_exp2RK(fs, e2);
    freeexp(fs, e2);
    freeexp(fs, e1);
    /* OP_EQ A B C   if ((RK(B) == RK(C)) ~= A) then pc++
     *
     */
    if (cond == 0 && op != OP_EQ) /* cond为0,false才跳转,这里事实上有三种情况
                                 * op -> OPR_NE, OPR_GT, OPR_GE
                                 */
    {
        int temp;  /* exchange args to replace by `<' or `<=' */
        temp = o1;
        o1 = o2;
        o2 = temp;  /* o1 <==> o2 */
        cond = 1;
        /* 这里为了节省指令的数量,做了等价替换 */
    }
    e1->u.info = condjump(fs, op, cond, o1, o2);
    e1->k = VJMP;
}

/* 编码单目运算
 * @param op 单目运算符
 * @param e 表达式
 */
void luaK_prefix (FuncState *fs, UnOpr op, expdesc *e, int line)
{
    expdesc e2;
    e2.t = e2.f = NO_JUMP;
    e2.k = VKNUM;
    e2.u.nval = 0;
    switch (op)
    {
        case OPR_MINUS:
        {
            if (isnumeral(e))  /* minus constant? */
                e->u.nval = luai_numunm(NULL, e->u.nval);  /* fold it */
            else
            {
                luaK_exp2anyreg(fs, e);
                codearith(fs, OP_UNM, e, &e2, line);
            }
            break;
        }
        case OPR_NOT:
            codenot(fs, e);
            break;
        case OPR_LEN: /* 计算字符长度 # */
        {
            luaK_exp2anyreg(fs, e);  /* cannot operate on constants */
            codearith(fs, OP_LEN, e, &e2, line);
            break;
        }
        default:
            lua_assert(0);
    }
}

/* 编码双目运算符
 * @param op 运算符种类
 * @param v 第一个操作数
 */
void luaK_infix (FuncState *fs, BinOpr op, expdesc *v)
{
    switch (op)
    {
        case OPR_AND:
        {
            /* 如果第一个操作数为false,要生成jump指令 */
            luaK_goiftrue(fs, v);
            break;
        }
        case OPR_OR:
        {
            /* 如果第一个操作数为true,要生成jump指令 */
            luaK_goiffalse(fs, v);
            break;
        }
        case OPR_CONCAT:
        {
            luaK_exp2nextreg(fs, v);  /* operand must be on the `stack' */
            break;
        }
        case OPR_ADD:
        case OPR_SUB:
        case OPR_MUL:
        case OPR_DIV:
        case OPR_MOD:
        case OPR_POW:
        {
            if (!isnumeral(v)) luaK_exp2RK(fs, v);
            break;
        }
        default:
        {
            luaK_exp2RK(fs, v);
            break;
        }
    }
}

/*
 * @param op 双目操作符
 * @param e1 左操作数
 * @param e2 右操作数
 */
void luaK_posfix (FuncState *fs, BinOpr op,
                  expdesc *e1, expdesc *e2, int line)
{
    switch (op)
    {
        case OPR_AND:
        {
            /* 保证左操作数的值为true */
            lua_assert(e1->t == NO_JUMP);  /* list must be closed */
            luaK_dischargevars(fs, e2);
            luaK_concat(fs, &e2->f, e1->f); /* 左操作数为false, */
            *e1 = *e2;
            break;
        }
        case OPR_OR:
        {
            /* 保证左操作数的值为false */
            lua_assert(e1->f == NO_JUMP);  /* list must be closed */
            luaK_dischargevars(fs, e2);
            luaK_concat(fs, &e2->t, e1->t); /* e1为 */
            *e1 = *e2;
            break;
        }
        case OPR_CONCAT:
        {
            luaK_exp2val(fs, e2);
            if (e2->k == VRELOCABLE && GET_OPCODE(getcode(fs, e2)) == OP_CONCAT)
            {
                lua_assert(e1->u.info == GETARG_B(getcode(fs, e2))-1);
                freeexp(fs, e1);
                SETARG_B(getcode(fs, e2), e1->u.info);
                e1->k = VRELOCABLE;
                e1->u.info = e2->u.info;
            }
            else
            {
                luaK_exp2nextreg(fs, e2);  /* operand must be on the 'stack' */
                codearith(fs, OP_CONCAT, e1, e2, line);
            }
            break;
        }
        case OPR_ADD:
        case OPR_SUB:
        case OPR_MUL:
        case OPR_DIV:
        case OPR_MOD:
        case OPR_POW:
        {
            codearith(fs, cast(OpCode, op - OPR_ADD + OP_ADD), e1, e2, line);
            break;
        }
        case OPR_EQ:
        case OPR_LT:
        case OPR_LE:
        {
            codecomp(fs, cast(OpCode, op - OPR_EQ + OP_EQ), 1, e1, e2);
            break;
        }
        case OPR_NE:
        case OPR_GT:
        case OPR_GE:
        {
            codecomp(fs, cast(OpCode, op - OPR_NE + OP_EQ), 0, e1, e2);
            break;
        }
        default:
            lua_assert(0);
    }
}

/* 记录下指令和代码的对应关系 */
void luaK_fixline (FuncState *fs, int line)
{
    fs->f->lineinfo[fs->pc - 1] = line;
}


void luaK_setlist (FuncState *fs, int base, int nelems, int tostore)
{
    int c =  (nelems - 1)/LFIELDS_PER_FLUSH + 1;
    int b = (tostore == LUA_MULTRET) ? 0 : tostore;
    lua_assert(tostore != 0);
    if (c <= MAXARG_C)
        luaK_codeABC(fs, OP_SETLIST, base, b, c);
    else if (c <= MAXARG_Ax)
    {
        luaK_codeABC(fs, OP_SETLIST, base, b, 0);
        codeextraarg(fs, c);
    }
    else
        luaX_syntaxerror(fs->ls, "constructor too long");
    fs->freereg = base + 1;  /* free registers with list values */
}

