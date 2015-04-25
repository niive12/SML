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
};

int main()
{
	Int n = 4*2;
    Doub rtol  = 0, atol = 2e-3, h1 = 1, hmin = 0.1, x1 = 0.0, x2 = 500.0;
	VecDoub ystart(n);
	ystart[0] = 0; // x1
	ystart[1] = 152870; // y1
	ystart[2] = 0; // x2
	ystart[3] = -153130; // y2
	ystart[4] = -1360278.1; // x1'
	ystart[5] = 0; // y1'
	ystart[6] = 1359122.8; // x2'
	ystart[7] = 0; // y2'
	Output out(-1);
	double g = 4.98e-10, M = 5.68e26, m1 = 9.2e18, m2 = m1;
	rhs d(g, M, m1, m2);
	Odeint< StepperStoerm<rhs> > ode(ystart, x1, x2, atol, rtol, h1, hmin, out, d);
	ode.integrate();


	double ddf_x1, ddf_y1, ddf_x2, ddf_y2, theta1, theta2, theta_diff;

	streambuf *coutbuf = std::cout.rdbuf();
	//* write to data.csv
	ofstream output_file("data.csv");
	cout.rdbuf(output_file.rdbuf());
	//  end write to data.csv */
	for(int i = 0; i < out.count; i++){
		ddf_x1 = out.ysave[0][i];
		ddf_y1 = out.ysave[1][i];
		ddf_x2 = out.ysave[2][i];
		ddf_y2 = out.ysave[3][i];
		theta1 = atan2(ddf_y1,ddf_x1);
		theta2 = atan2(ddf_y2,ddf_x2);

		theta_diff = (theta1 - theta2);
		while(theta_diff < -M_PI){
			theta_diff += 2*M_PI;
		}
		while(theta_diff > M_PI){
			theta_diff -= 2*M_PI;
		}
		theta_diff = abs(theta_diff);
		cout << out.xsave[i] << ", "
			 << sqrt(pow(ddf_x1,2) + pow(ddf_y1,2)) << ", "
			 << sqrt(pow(ddf_x2,2) + pow(ddf_y2,2)) << ", "
			 << theta_diff << endl;
	}
	cout.rdbuf(coutbuf);

	cout << "Number of iterations: " << out.count << endl;
	cout << "Worst case error: " << out.count*atol << endl;
	return 0;
}
