#include <iostream>
#include <iomanip>
#include <cmath>
#include <vector>
#include "../NR_LIB/code/nr3.h"
#include "../NR_LIB/code/ludcmp.h"
#include "../NR_LIB/code/odeint.h"
#include "../NR_LIB/code/stepper.h"
#include "../NR_LIB/code/stepperbs.h"
#include "../NR_LIB/code/stepperross.h"
#include "../NR_LIB/code/stepperdopr5.h"

#define FIRST 0
#define SECOND 1
#define RUNMODE SECOND


#define EULER 0
#define LEAP_FROG 1

using namespace std;

#if RUNMODE == FIRST
vector<double> y_marks(vector<double> prev){
	vector<double> res(2);

	res[0] = prev[0]*prev[1];
	res[1] = -pow(prev[0],2);

	return res;
}

template <class T>
vector<double> ode_method(T &func, vector<double> init, int N, double h, int method){

	vector<double> x_vals_prev = init;
	vector<double> x_vals_new = init;

	double mark;

	for(int n = 0; n < N; n++){
		for(int i = 0; i < 2; i++){
			if(method == EULER){
				mark = func(x_vals_prev)[i];
				x_vals_new[i] = x_vals_prev[i] + h*(mark);
				x_vals_prev[i] = x_vals_new[i];
			} else if(method == LEAP_FROG){
				mark = func(x_vals_prev)[i];
				double temp = x_vals_new[i];
				x_vals_new[i] = x_vals_prev[i] + 2*h*mark;
				x_vals_prev[i] = temp;
			}
		}
	}

	return x_vals_new;
}
#endif


struct rhs{
    rhs(){}
    void operator()(const Doub x, VecDoub_I &y, VecDoub_O &dydx){
        dydx[0] = y[0]*y[1];
        dydx[1] = - pow(y[0],2);
    }

    void jacobian(Doub x, VecDoub_I &y, VecDoub_O &dfdx, MatDoub_O &dfdy){
        int n = y.size();
        // set dfdx zero
        for(int i = n; i < n; i++){
            dfdx[i] = 0;
        }
        // set dfdy
        dfdy[0][0] = y[1];
        dfdy[0][1] = y[0];
        dfdy[1][0] = -2*y[1];
        dfdy[1][1] = 0;
    }

};



int main()
{

#if RUNMODE == FIRST
    // make eulers!
    // output h, y_0^h(x),
    cout << "Hello Euler!" << endl;

	vector<double> initials(2);
	initials[0] = 1;
	initials[1] = 1;

	vector<double> result_h1(2);
	vector<double> result_h2(0,2);
	vector<double> result_h3(0,2);


	double X = 10;

	cout << "For X = " << X << endl;

	int width = 20;
	cout << fixed << setprecision(width-5);

	cout << setw(width) << "h\t"
		 << setw(width) << "y[0]\t"
		 << setw(width) << "error y[0]\t"
		 << setw(width) << "y[1]\t"
		 << setw(width) << "error y[1]\t"
		 << setw(width) << "y[0]^2 + y[1]^2"
		 << endl;



	for(int N = 5*X; N <= 40960*X; N = N*2){
		double h = X/double(N);

		result_h1 = ode_method(y_marks, initials, N, h, LEAP_FROG);

		if(N >= 40*X)
			cout << setw(width) << h << "\t"
				 << setw(width) << result_h1[0] << "\t"
				 << setw(width) << (result_h3[0] - result_h2[0])/(result_h2[0]-result_h1[0]) << "\t"
				 << setw(width) << result_h1[1] << "\t"
				 << setw(width) << (result_h3[1] - result_h2[1])/(result_h2[1]-result_h1[1]) << "\t"
				 << endl;

		result_h3 = result_h2;
		result_h2 = result_h1;
    }
#elif RUNMODE == SECOND
//    Numerically integrate the ODE system given by:
//    y_1’(x)=y_1(x)y_2(x)
//    y_2’(x)=-y_1(x)^2
//    with starting condition:
//    y_1(0)=y_2_(0) = 1
//    integrate from x=0 to x=1 and print the number of calculations of the right side for a absolue max error of 10^-6, y_1(1) and y1(1)^2+y_2(1)^2
//    Use the Odeint framework in NRcode and try out the StepperDopr5 and StepperRoss as the step functions.

    Int n = 2;
    Doub rtol  = 0, atol = 1.0e-6, h1 = 0.01, hmin = 0.0, x1 = 0.0, x2 = 1;
    VecDoub ystart(n);
    ystart[0] = 1;
    ystart[1] = 1;
    Output out(20);
    rhs d;
    Odeint< StepperRoss<rhs> > ode(ystart, x1, x2, atol, rtol, h1, hmin, out, d);
    ode.integrate();

    double y1, y2;

    for(int i = 0; i < out.count; i++){
        y1 = out.ysave[0][i];
        y2 = out.ysave[1][i];
        cout << out.xsave[i] << " "
             << y1 << " "
             << pow(y1,2) + pow(y2,2) << endl;
    }


#endif

	return 0;
}

