#include <iostream>
#include <stdlib.h>
#include <cmath>
#include "../NR_LIB/code/nr3.h"
#include "../NR_LIB/code/odeint.h"
#include "../NR_LIB/code/stepper.h"
#include "../NR_LIB/code/stepperdopr853.h"
#include "../NR_LIB/code/shoot.h"
#include "../NR_LIB/code/stepperdopr5.h"

struct Rhs {
    Rhs(){}
    void operator()(const Doub x, VecDoub_I &y, VecDoub_O &dydx){
        dydx[0] = y[1];
        dydx[1] = sqrt(1 + pow(y[1],2));
    }
};

struct Load {

};


int main(int argc, char** argv)
{
	//u(x) for 0<=x<=1
	//u''(x) = sqrt(1+pow(u'(x),2))

	//u(0) = 0 , u(1) = 1

    // y_0(t) = u(t)
    // y_1(t) = u`(t)

    // y_0`(t) = y_1(t)
    // y_1`(t) = sqrt(1 + (y_1(t))^2)

	return EXIT_SUCCESS;
}
