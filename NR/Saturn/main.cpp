#include <iostream>
#include <iomanip>
#include <cmath>
#include <vector>
#include "../NR_LIB/code/nr3.h"
#include "../NR_LIB/code/ludcmp.h"
#include "../NR_LIB/code/odeint.h"
#include "../NR_LIB/code/stepper.h"
#include "../NR_LIB/code/stepperbs.h"
#include "../NR_LIB/code/stepperstoerm.h"


using namespace std;


struct rhs{
    double g, M, m1, m2;
    rhs(double kg, double kM, double km1, double km2) : g(kg), M(kM), m1(km1), m2(km2){}
    void operator()(const Doub x, VecDoub_I &y, VecDoub_O &dydx){
        double r12 = sqrt(pow(y[0] - y[2],2) + pow(y[1] - y[2],2));
        double r1 = sqrt(pow(y[0],2) + pow(y[1],2));
        double r2 = sqrt(pow(y[2],2) + pow(y[3],2));

        dydx[0] = ;
        dydx[1] = ;
        dydx[2] = ;
        dydx[3] = ;
        dydx[4] = ;
        dydx[5] = ;
        dydx[6] = ;
        dydx[7] =;
    }

//    void jacobian(Doub x, VecDoub_I &y, VecDoub_O &dfdx, MatDoub_O &dfdy){
//        int n = y.size();
//        // set dfdx zero
//        for(int i = n; i < n; i++){
//            dfdx[i] = 0;
//        }
//        // set dfdy
//        dfdy[0][0] = y[1];
//        dfdy[0][1] = y[0];
//        dfdy[1][0] = -2*y[1];
//        dfdy[1][1] = 0;
//    }

};



int main()
{

//    Numerically integrate the ODE system given by:
//    y_1’(x)=y_1(x)y_2(x)
//    y_2’(x)=-y_1(x)^2
//    with starting condition:
//    y_1(0)=y_2_(0) = 1
//    integrate from x=0 to x=1 and print the number of calculations of the right side for a absolue max error of 10^-6, y_1(1) and y1(1)^2+y_2(1)^2
//    Use the Odeint framework in NRcode and try out the StepperDopr5 and StepperRoss as the step functions.

    Int n = 4*2;
    Doub rtol  = 0, atol = 10, h1 = 1, hmin = 0.0, x1 = 0.0, x2 = 1.0;
    VecDoub ystart(n);
    ystart[0] = 0; // x1
    ystart[1] = 152879; // y1
    ystart[2] = 0; // x2
    ystart[3] = -153130; // y2
    ystart[4] = -1360278.1; // x1'
    ystart[5] = 0; // y1'
    ystart[6] = 1359122.8; // x2'
    ystart[7] = 0; // y2'
    Output out(100);
    rhs d;
    Odeint< StepperDopr5<rhs> > ode(ystart, x1, x2, atol, rtol, h1, hmin, out, d);
    ode.integrate();

    double y1, y2;

    for(int i = 0; i < out.count; i++){
        y1 = out.ysave[0][i];
        y2 = out.ysave[1][i];
        cout << out.xsave[i] << " "
             << out.ysave[0][i] << " "
             << out.ysave[1][i] << " "
             << out.ysave[2][i] << " "
             << out.ysave[3][i] << endl;
    }



    return 0;
}

