#include <iostream>
#include "../NR_LIB/code/nr3.h"
#include "../NR_LIB/code/odeint.h"
#include "../NR_LIB/code/shoot.h"
#include "../NR_LIB/code/sphoot.h"

struct Rhs {
    Rhs(){}
    void operator()(const Doub x, VecDoub_I &y, VecDoub_O &dydx){
        dydx[0] = 0;
        dydx[1] = 0;
    }
};

int main()
{

    return true;
}
