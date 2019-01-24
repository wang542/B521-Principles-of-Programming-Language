void *env, *envr__m__y, *k, *closurr_, *closr__m__y, *v, *code;

void (*pc)();

struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_n; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_nexpr1; void *_nexpr2; } _mult;
    struct { void *_nexp; } _subr1;
    struct { void *_nexp; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_kexp; void *_vexp; } _throw;
    struct { void *_exp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *n);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *nexpr1, void *nexpr2);
void *exprr_subr1(void *nexp);
void *exprr_zero(void *nexp);
void *exprr_letcc(void *body);
void *exprr_throw(void *kexp, void *vexp);
void *exprr_let(void *exp, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

struct envri;
typedef struct envri envri;
struct envri {
  enum {
    _emptyr__m__env_envri,
    _extendr__m__env_envri
  } tag;
  union {
    struct { char dummy; } _emptyr__m__env;
    struct { void *_x; void *_val; } _extendr__m__env;
  } u;
};

void *envrir_emptyr__m__env();
void *envrir_extendr__m__env(void *x, void *val);

void applyr__m__env();
struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _closur_clos
  } tag;
  union {
    struct { void *_body; void *_env; } _closur;
  } u;
};

void *closr_closur(void *body, void *env);

void applyr__m__closure();
struct ktniu;
typedef struct ktniu ktniu;
struct ktniu {
  enum {
    _emptyr__m__k_ktniu,
    _multr__m__innerk_ktniu,
    _multr__m__outerk_ktniu,
    _subr1r__m__outerk_ktniu,
    _zeror__m__outerk_ktniu,
    _conseqr__m__innerk_ktniu,
    _altr__m__innerk_ktniu,
    _ifr__m__outerk_ktniu,
    _throwr__m__innerk_ktniu,
    _throwr__m__outerk_ktniu,
    _letr__m__outerk_ktniu,
    _appr__m__innerk_ktniu,
    _appr__m__outerk_ktniu
  } tag;
  union {
    struct { void *_dismount; } _emptyr__m__k;
    struct { void *_xr1envr__ex__; void *_kr__ex__; } _multr__m__innerk;
    struct { void *_xr2r__ex__; void *_envr__ex__; void *_kr__ex__; } _multr__m__outerk;
    struct { void *_kr__ex__; } _subr1r__m__outerk;
    struct { void *_kr__ex__; } _zeror__m__outerk;
    struct { void *_kr__ex__; } _conseqr__m__innerk;
    struct { void *_kr__ex__; } _altr__m__innerk;
    struct { void *_conseqr__ex__; void *_altr__ex__; void *_envr__ex__; void *_kr__ex__; } _ifr__m__outerk;
    struct { void *_kenvr__ex__; } _throwr__m__innerk;
    struct { void *_vr__m__expr__ex__; void *_envr__ex__; } _throwr__m__outerk;
    struct { void *_body; void *_envr__ex__; void *_kr__ex__; } _letr__m__outerk;
    struct { void *_ratorenvr__ex__; void *_kr__ex__; } _appr__m__innerk;
    struct { void *_randr__ex__; void *_envr__ex__; void *_kr__ex__; } _appr__m__outerk;
  } u;
};

void *ktniur_emptyr__m__k(void *dismount);
void *ktniur_multr__m__innerk(void *xr1envr__ex__, void *kr__ex__);
void *ktniur_multr__m__outerk(void *xr2r__ex__, void *envr__ex__, void *kr__ex__);
void *ktniur_subr1r__m__outerk(void *kr__ex__);
void *ktniur_zeror__m__outerk(void *kr__ex__);
void *ktniur_conseqr__m__innerk(void *kr__ex__);
void *ktniur_altr__m__innerk(void *kr__ex__);
void *ktniur_ifr__m__outerk(void *conseqr__ex__, void *altr__ex__, void *envr__ex__, void *kr__ex__);
void *ktniur_throwr__m__innerk(void *kenvr__ex__);
void *ktniur_throwr__m__outerk(void *vr__m__expr__ex__, void *envr__ex__);
void *ktniur_letr__m__outerk(void *body, void *envr__ex__, void *kr__ex__);
void *ktniur_appr__m__innerk(void *ratorenvr__ex__, void *kr__ex__);
void *ktniur_appr__m__outerk(void *randr__ex__, void *envr__ex__, void *kr__ex__);

void applyr__m__k();
void valuer__m__ofr__m__cps();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

