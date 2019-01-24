#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a9test.h"

void *ktniur_emptyr__m__k(void *dismount) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__k_ktniu;
  _data->u._emptyr__m__k._dismount = dismount;
  return (void *)_data;
}

void *ktniur_multr__m__innerk(void *xr1envr__ex__, void *kr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__innerk_ktniu;
  _data->u._multr__m__innerk._xr1envr__ex__ = xr1envr__ex__;
  _data->u._multr__m__innerk._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktniur_multr__m__outerk(void *xr2r__ex__, void *envr__ex__, void *kr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _multr__m__outerk_ktniu;
  _data->u._multr__m__outerk._xr2r__ex__ = xr2r__ex__;
  _data->u._multr__m__outerk._envr__ex__ = envr__ex__;
  _data->u._multr__m__outerk._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktniur_subr1r__m__outerk(void *kr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1r__m__outerk_ktniu;
  _data->u._subr1r__m__outerk._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktniur_zeror__m__outerk(void *kr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zeror__m__outerk_ktniu;
  _data->u._zeror__m__outerk._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktniur_conseqr__m__innerk(void *kr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _conseqr__m__innerk_ktniu;
  _data->u._conseqr__m__innerk._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktniur_altr__m__innerk(void *kr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _altr__m__innerk_ktniu;
  _data->u._altr__m__innerk._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktniur_ifr__m__outerk(void *conseqr__ex__, void *altr__ex__, void *envr__ex__, void *kr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _ifr__m__outerk_ktniu;
  _data->u._ifr__m__outerk._conseqr__ex__ = conseqr__ex__;
  _data->u._ifr__m__outerk._altr__ex__ = altr__ex__;
  _data->u._ifr__m__outerk._envr__ex__ = envr__ex__;
  _data->u._ifr__m__outerk._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktniur_throwr__m__innerk(void *kenvr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throwr__m__innerk_ktniu;
  _data->u._throwr__m__innerk._kenvr__ex__ = kenvr__ex__;
  return (void *)_data;
}

void *ktniur_throwr__m__outerk(void *vr__m__expr__ex__, void *envr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throwr__m__outerk_ktniu;
  _data->u._throwr__m__outerk._vr__m__expr__ex__ = vr__m__expr__ex__;
  _data->u._throwr__m__outerk._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *ktniur_letr__m__outerk(void *body, void *envr__ex__, void *kr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letr__m__outerk_ktniu;
  _data->u._letr__m__outerk._body = body;
  _data->u._letr__m__outerk._envr__ex__ = envr__ex__;
  _data->u._letr__m__outerk._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktniur_appr__m__innerk(void *ratorenvr__ex__, void *kr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _appr__m__innerk_ktniu;
  _data->u._appr__m__innerk._ratorenvr__ex__ = ratorenvr__ex__;
  _data->u._appr__m__innerk._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktniur_appr__m__outerk(void *randr__ex__, void *envr__ex__, void *kr__ex__) {
ktniu* _data = (ktniu*)malloc(sizeof(ktniu));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _appr__m__outerk_ktniu;
  _data->u._appr__m__outerk._randr__ex__ = randr__ex__;
  _data->u._appr__m__outerk._envr__ex__ = envr__ex__;
  _data->u._appr__m__outerk._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *closr_closur(void *body, void *env) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _closur_clos;
  _data->u._closur._body = body;
  _data->u._closur._env = env;
  return (void *)_data;
}

void *envrir_emptyr__m__env() {
envri* _data = (envri*)malloc(sizeof(envri));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__env_envri;
  return (void *)_data;
}

void *envrir_extendr__m__env(void *x, void *val) {
envri* _data = (envri*)malloc(sizeof(envri));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extendr__m__env_envri;
  _data->u._extendr__m__env._x = x;
  _data->u._extendr__m__env._val = val;
  return (void *)_data;
}

void *exprr_const(void *cexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *n) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *nexpr1, void *nexpr2) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *)_data;
}

void *exprr_subr1(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *)_data;
}

void *exprr_zero(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *)_data;
}

void *exprr_letcc(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *exprr_throw(void *kexp, void *vexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *exp, void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
code = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)),exprr_const((void *)1),exprr_mult(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_subr1(exprr_var((void *)0))))))),exprr_mult(exprr_letcc(exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_throw(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_const((void *)4))))),exprr_const((void *)5)));
env = (void *)envrir_emptyr__m__env();
pc = &valuer__m__ofr__m__cps;
mount_tram();
printf("Fact 5: %d\n", (int)v);}

void valuer__m__ofr__m__cps()
{
expr* _c = (expr*)code;
switch (_c->tag) {
case _const_expr: {
void *exp = _c->u._const._cexp;
v = (void *)exp;
pc = &applyr__m__k;
break; }
case _mult_expr: {
void *xr1 = _c->u._mult._nexpr1;
void *xr2 = _c->u._mult._nexpr2;
k = (void *)ktniur_multr__m__outerk(xr2,env,k);
code = (void *)xr1;
pc = &valuer__m__ofr__m__cps;
break; }
case _subr1_expr: {
void *x = _c->u._subr1._nexp;
k = (void *)ktniur_subr1r__m__outerk(k);
code = (void *)x;
pc = &valuer__m__ofr__m__cps;
break; }
case _zero_expr: {
void *x = _c->u._zero._nexp;
k = (void *)ktniur_zeror__m__outerk(k);
code = (void *)x;
pc = &valuer__m__ofr__m__cps;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
k = (void *)ktniur_ifr__m__outerk(conseq,alt,env,k);
code = (void *)test;
pc = &valuer__m__ofr__m__cps;
break; }
case _letcc_expr: {
void *body = _c->u._letcc._body;
env = (void *)envrir_extendr__m__env(env,k);
code = (void *)body;
pc = &valuer__m__ofr__m__cps;
break; }
case _throw_expr: {
void *kr__m__exp = _c->u._throw._kexp;
void *vr__m__exp = _c->u._throw._vexp;
k = (void *)ktniur_throwr__m__outerk(vr__m__exp,env);
code = (void *)kr__m__exp;
pc = &valuer__m__ofr__m__cps;
break; }
case _let_expr: {
void *e = _c->u._let._exp;
void *body = _c->u._let._body;
k = (void *)ktniur_letr__m__outerk(body,env,k);
code = (void *)e;
pc = &valuer__m__ofr__m__cps;
break; }
case _var_expr: {
void *expr = _c->u._var._n;
envr__m__y = (void *)expr;
pc = &applyr__m__env;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
v = (void *)closr_closur(body,env);
pc = &applyr__m__k;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
k = (void *)ktniur_appr__m__outerk(rand,env,k);
code = (void *)rator;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__k()
{
ktniu* _c = (ktniu*)k;
switch (_c->tag) {
case _emptyr__m__k_ktniu: {
void *dismount = _c->u._emptyr__m__k._dismount;
_trstr *trstr = (_trstr *)dismount;
longjmp(*trstr->jmpbuf, 1);
break; }
case _multr__m__innerk_ktniu: {
void *xr1envr__ex__ = _c->u._multr__m__innerk._xr1envr__ex__;
void *kr__ex__ = _c->u._multr__m__innerk._kr__ex__;
v = (void *)(void *)((int)xr1envr__ex__ * (int)v);
k = (void *)kr__ex__;
pc = &applyr__m__k;
break; }
case _multr__m__outerk_ktniu: {
void *xr2r__ex__ = _c->u._multr__m__outerk._xr2r__ex__;
void *envr__ex__ = _c->u._multr__m__outerk._envr__ex__;
void *kr__ex__ = _c->u._multr__m__outerk._kr__ex__;
k = (void *)ktniur_multr__m__innerk(v,kr__ex__);
code = (void *)xr2r__ex__;
env = (void *)envr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
case _subr1r__m__outerk_ktniu: {
void *kr__ex__ = _c->u._subr1r__m__outerk._kr__ex__;
v = (void *)(void *)((int)v - 1);
k = (void *)kr__ex__;
pc = &applyr__m__k;
break; }
case _zeror__m__outerk_ktniu: {
void *kr__ex__ = _c->u._zeror__m__outerk._kr__ex__;
v = (void *)(v == 0);
k = (void *)kr__ex__;
pc = &applyr__m__k;
break; }
case _conseqr__m__innerk_ktniu: {
void *kr__ex__ = _c->u._conseqr__m__innerk._kr__ex__;
k = (void *)kr__ex__;
pc = &applyr__m__k;
break; }
case _altr__m__innerk_ktniu: {
void *kr__ex__ = _c->u._altr__m__innerk._kr__ex__;
k = (void *)kr__ex__;
pc = &applyr__m__k;
break; }
case _ifr__m__outerk_ktniu: {
void *conseqr__ex__ = _c->u._ifr__m__outerk._conseqr__ex__;
void *altr__ex__ = _c->u._ifr__m__outerk._altr__ex__;
void *envr__ex__ = _c->u._ifr__m__outerk._envr__ex__;
void *kr__ex__ = _c->u._ifr__m__outerk._kr__ex__;
if(v) {
  k = (void *)ktniur_conseqr__m__innerk(kr__ex__);
code = (void *)conseqr__ex__;
env = (void *)envr__ex__;
pc = &valuer__m__ofr__m__cps;

} else {
  k = (void *)ktniur_altr__m__innerk(kr__ex__);
code = (void *)altr__ex__;
env = (void *)envr__ex__;
pc = &valuer__m__ofr__m__cps;

}
break; }
case _throwr__m__innerk_ktniu: {
void *kenvr__ex__ = _c->u._throwr__m__innerk._kenvr__ex__;
k = (void *)kenvr__ex__;
pc = &applyr__m__k;
break; }
case _throwr__m__outerk_ktniu: {
void *vr__m__expr__ex__ = _c->u._throwr__m__outerk._vr__m__expr__ex__;
void *envr__ex__ = _c->u._throwr__m__outerk._envr__ex__;
k = (void *)ktniur_throwr__m__innerk(v);
code = (void *)vr__m__expr__ex__;
env = (void *)envr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
case _letr__m__outerk_ktniu: {
void *bodyr__ex__ = _c->u._letr__m__outerk._body;
void *envr__ex__ = _c->u._letr__m__outerk._envr__ex__;
void *kr__ex__ = _c->u._letr__m__outerk._kr__ex__;
env = (void *)envrir_extendr__m__env(envr__ex__,v);
k = (void *)kr__ex__;
code = (void *)bodyr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
case _appr__m__innerk_ktniu: {
void *ratorenvr__ex__ = _c->u._appr__m__innerk._ratorenvr__ex__;
void *kr__ex__ = _c->u._appr__m__innerk._kr__ex__;
closurr_ = (void *)ratorenvr__ex__;
closr__m__y = (void *)v;
k = (void *)kr__ex__;
pc = &applyr__m__closure;
break; }
case _appr__m__outerk_ktniu: {
void *randr__ex__ = _c->u._appr__m__outerk._randr__ex__;
void *envr__ex__ = _c->u._appr__m__outerk._envr__ex__;
void *kr__ex__ = _c->u._appr__m__outerk._kr__ex__;
k = (void *)ktniur_appr__m__innerk(v,kr__ex__);
code = (void *)randr__ex__;
env = (void *)envr__ex__;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__closure()
{
clos* _c = (clos*)closurr_;
switch (_c->tag) {
case _closur_clos: {
void *body = _c->u._closur._body;
void *envr__ex__ = _c->u._closur._env;
env = (void *)envrir_extendr__m__env(envr__ex__,closr__m__y);
code = (void *)body;
pc = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
envri* _c = (envri*)env;
switch (_c->tag) {
case _emptyr__m__env_envri: {
fprintf(stderr, "unbound variable");
 exit(1);
break; }
case _extendr__m__env_envri: {
void *x = _c->u._extendr__m__env._x;
void *val = _c->u._extendr__m__env._val;
if((envr__m__y == 0)) {
  v = (void *)val;
pc = &applyr__m__k;

} else {
  envr__m__y = (void *)(void *)((int)envr__m__y - 1);
env = (void *)x;
pc = &applyr__m__env;

}
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
k= (void *)ktniur_emptyr__m__k(dismount);
for(;;) {
pc();
}
}
return 0;
}
