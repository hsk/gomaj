#include "../lib/core.h"
#include <stdio.h>
int E_classId = Class_genId();
struct E{

  int id;
  E ():id(E_classId){

  }
};


int EInt_classId = Class_genId();
struct EInt:E{

  EInt (int x):x(x){
    (id = EInt_classId);
{

    }
  }
  int x;
};


int EAdd_classId = Class_genId();
struct EAdd:E{

  EAdd (E* x, E* y):x(x), y(y){
    (id = EAdd_classId);
{

    }
  }
  E* x;
  E* y;
};


int EMul_classId = Class_genId();
struct EMul:E{

  EMul (E* x, E* y):x(x), y(y){
    (id = EMul_classId);
{

    }
  }
  E* x;
  E* y;
};


struct Eval{

  int(*eval)(Class*);
};

Vec* Eval_v = newVec();

int Eval_EInt_eval(Class* self_) {
  EInt* self = ((EInt*)self_);
  return (self -> x);
} 
Eval* newEval_EInt() {
  Eval (* impl) = (new Eval());
  setVec(Eval_v, EInt_classId, ((void*)impl));
  ((impl -> eval) = (& Eval_EInt_eval));
  return impl;
} 
Eval* Eval_EInt_ = newEval_EInt();

int Eval_EAdd_eval(Class* self_) {
  EAdd* self = ((EAdd*)self_);
  return ((((Eval*)(Eval_v -> data)[((* (self -> x)) . id)]) -> eval)(((Class*)(& (* (self -> x))))) + (((Eval*)(Eval_v -> data)[((* (self -> y)) . id)]) -> eval)(((Class*)(& (* (self -> y))))));
} 
Eval* newEval_EAdd() {
  Eval (* impl) = (new Eval());
  setVec(Eval_v, EAdd_classId, ((void*)impl));
  ((impl -> eval) = (& Eval_EAdd_eval));
  return impl;
} 
Eval* Eval_EAdd_ = newEval_EAdd();

int Eval_EMul_eval(Class* self_) {
  EMul* self = ((EMul*)self_);
  return ((((Eval*)(Eval_v -> data)[((* (self -> x)) . id)]) -> eval)(((Class*)(& (* (self -> x))))) * (((Eval*)(Eval_v -> data)[((* (self -> y)) . id)]) -> eval)(((Class*)(& (* (self -> y))))));
} 
Eval* newEval_EMul() {
  Eval (* impl) = (new Eval());
  setVec(Eval_v, EMul_classId, ((void*)impl));
  ((impl -> eval) = (& Eval_EMul_eval));
  return impl;
} 
Eval* Eval_EMul_ = newEval_EMul();

int main() {
  EInt i(1);
  printf("eval 1 = %d\n", (((Eval*)(Eval_v -> data)[(i . id)]) -> eval)(((Class*)(& i))));
  EAdd add((new EInt(1)), (new EInt(2)));
  printf("eval 1 + 2 = %d\n", (((Eval*)(Eval_v -> data)[(add . id)]) -> eval)(((Class*)(& add))));
  EMul mul((new EAdd((new EInt(1)), (new EInt(2)))), (new EInt(111)));
  printf("eval (1 + 2) * 111 = %d\n", (((Eval*)(Eval_v -> data)[(mul . id)]) -> eval)(((Class*)(& mul))));
  return 0;
} 
