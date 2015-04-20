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
		double r12 = sqrt(pow(y[0] - y[2],2) + pow(y[1] - y[3],2));
		double r1 = sqrt(pow(y[0],2) + pow(y[1],2));
		double r2 = sqrt(pow(y[2],2) + pow(y[3],2));

		dydx[0] = -M*g*y[0]/pow(r1,3) + m2*g*(y[2] - y[0])/pow(r12,3);
		dydx[1] = -M*g*y[1]/pow(r1,3) + m2*g*(y[3] - y[1])/pow(r12,3);
		dydx[2] = -M*g*y[2]/pow(r2,3) - m1*g*(y[2] - y[0])/pow(r12,3);
		dydx[3] = -M*g*y[3]/pow(r2,3) - m1*g*(y[3] - y[1])/pow(r12,3);
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
    Doub rtol  = 0, atol = 1e-6, h1 = 1, hmin = 0.0, x1 = 0.0, x2 = 200.0;
    VecDoub ystart(n);
    ystart[0] = 0; // x1
    ystart[1] = 152870; // y1
    ystart[2] = 0; // x2
    ystart[3] = -153130; // y2
    ystart[4] = -1360278.1; // x1'
    ystart[5] = 0; // y1'
    ystart[6] = 1359122.8; // x2'
    ystart[7] = 0; // y2'
    Output out(100);
    double g = 4.98e-10, M = 5.68e26, m1 = 9.2e18, m2 = m1;
    rhs d(g, M, m1, m2);
    Odeint< StepperStoerm<rhs> > ode(ystart, x1, x2, atol, rtol, h1, hmin, out, d);
    ode.integrate();

    double ddf_x1, ddf_y1, ddf_x2, ddf_y2, theta1, theta2;

    for(int i = 0; i < out.count; i++){
        ddf_x1 = out.ysave[0][i];
        ddf_y1 = out.ysave[1][i];
        ddf_x2 = out.ysave[2][i];
        ddf_y2 = out.ysave[3][i];
        theta1 = atan(ddf_y1/ddf_x1);
        theta2 = atan(ddf_y2/ddf_x2);

        cout << out.xsave[i] << ", "
             << sqrt(pow(ddf_x1,2) + pow(ddf_y1,2)) << ", "
             << sqrt(pow(ddf_x2,2) + pow(ddf_y2,2)) << ", "
             << theta1 << ", "
             << theta2 << endl;

    }



    return 0;
}

