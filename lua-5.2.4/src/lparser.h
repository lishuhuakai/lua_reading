/*
** $Id: lparser.h,v 1.70.1.1 2013/04/12 18:48:47 roberto Exp $
** Lua Parser
** See Copyright Notice in lua.h
*/

#ifndef lparser_h
#define lparser_h

#include "llimits.h"
#include "lobject.h"
#include "lzio.h"
/* 语法分析器 */

/*
** Expression descriptor
*/

typedef enum
{
    VVOID,    /* no value */
    VNIL,     /* 值为nil */
    VTRUE,    /* 值为true */
    VFALSE,   /* 值为false */
    VK,       /* info = index of constant in `k' -- 值在常量表中 */
    VKNUM,    /* nval = numerical value -- 表达式为常量值 */
    /* 表达式结果已经加载至寄存器,可以通过u.info来访问该寄存器 */
    VNONRELOC,    /* info = result register */
    VLOCAL,   /* info = local register */
    VUPVAL,       /* info = index of upvalue in 'upvalues' */
    VINDEXED, /* t = table register/upvalue; idx = index R/K */
    VJMP,     /* info = instruction pc */
    /* 表达式的结果需要加载至寄存器中 */
    VRELOCABLE,   /* info = instruction pc */
    /* 表达式对应一个函数调用 */
    VCALL,    /* info = instruction pc */
    /* 表达式对应可变参数 */
    VVARARG   /* info = instruction pc */
} expkind;


#define vkisvar(k)  (VLOCAL <= (k) && (k) <= VINDEXED)
#define vkisinreg(k)    ((k) == VNONRELOC || (k) == VLOCAL)

/* 表达式的描述 */
typedef struct expdesc
{
    expkind k;  /* 类型 */
    union
    {
        struct    /* for indexed variables (VINDEXED) */
        {
            short idx;  /* index (R/K) */
            lu_byte t;  /* table (register or upvalue) */
            lu_byte vt;  /* whether 't' is register (VLOCAL) or upvalue (VUPVAL) */
        } ind;
        int info;  /* for generic use */
        lua_Number nval;  /* for VKNUM */
    } u;
    int t;  /* patch list of `exit when true' */
    int f;  /* patch list of `exit when false' */
} expdesc;


/* description of active local variable */
typedef struct Vardesc
{
    short idx;  /* variable index in stack */
} Vardesc;


/* description of pending goto statements and label statements */
typedef struct Labeldesc
{
    TString *name;  /* label identifier */
    int pc;  /* position in code */
    int line;  /* line where it appeared */
    lu_byte nactvar;  /* local level where it appears in current block */
} Labeldesc;


/* list of labels or gotos */
typedef struct Labellist
{
    Labeldesc *arr;  /* array */
    int n;  /* number of entries in use */
    int size;  /* array size */
} Labellist;


/* dynamic structures used by the parser */
typedef struct Dyndata
{
    struct    /* list of active local variables */
    {
        Vardesc *arr;
        int n;
        int size;
    } actvar;
    Labellist gt;  /* list of pending gotos */
    Labellist label;   /* list of active labels */
} Dyndata;


/* control of blocks */
struct BlockCnt;  /* defined in lparser.c */


/* state needed to generate code for a given function */
/* 用于生成指令的辅助结构,每一个函数都会使用这样一个结构 */
typedef struct FuncState 
{
    Proto *f;  /* current function header */
    Table *h;  /* table to find (and reuse) elements in `k' */
    struct FuncState *prev;  /* enclosing function */
    struct LexState *ls;  /* lexical state */
    struct BlockCnt *bl;  /* chain of current blocks */
    int pc;  /* next position to code (equivalent to `ncode') */
    int lasttarget;   /* 'label' of last 'jump label' */
    int jpc;  /* list of pending jumps to `pc' */
    int nk;  /* number of elements in `k' */
    int np;  /* number of elements in `p' */
    int firstlocal;  /* index of first local var (in Dyndata array) */
    short nlocvars;  /* number of elements in 'f->locvars' */
    lu_byte nactvar;  /* number of active local variables */
    lu_byte nups;  /* number of upvalues */
    lu_byte freereg;  /* first free register */
} FuncState;


LUAI_FUNC Closure *luaY_parser (lua_State *L, ZIO *z, Mbuffer *buff,
                                Dyndata *dyd, const char *name, int firstchar);


#endif
