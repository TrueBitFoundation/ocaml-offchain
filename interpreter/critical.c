
#include <stdint.h>

extern uint32_t getStep();
extern uint32_t getTarget();
extern uint32_t *getStack();

extern void setStep(uint32_t);
extern void setStack(uint32_t *);

// Push function or loop
void __env_pushCritical(uint32_t loc) {
    uint32_t target = getTarget();
    uint32_t step = getStep();
    if (target <= step) return;
    uint32_t *stack = getStack();
    stack[0] = step;
    stack[1] = loc;
    setStack(stack+2);
    setStep(step+1);
}

// Pop function
void __env_popCritical() {
    uint32_t target = getTarget();
    uint32_t step = getStep();
    if (target <= step) return;
    uint32_t *stack = getStack();
    setStack(stack-2);
    setStep(step+1);
}

// Pop loop
void __env_popLoopCritical(uint32_t loc) {
    uint32_t target = getTarget();
    uint32_t step = getStep();
    if (target <= step) return;
    uint32_t *stack = getStack() - 2;
    if (stack[1] != loc) return;
    setStack(stack);
    setStep(step+1);
}

