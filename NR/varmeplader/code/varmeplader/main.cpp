#include <iostream>
#include <cmath>
#include "../../../NR_LIB/code/nr3.h"
#include "../../../NR_LIB/code/quadrature.h"

double func_F_x(double x){
	const double d = 1.0;
	double y = 0;
	return 0.5* (pow(d,2)/pow((pow(d,2)+pow(x-y,2)),3/2));
}

double func_F_y(double y){
	const double d = 1.0;
	double x = 0;
	return 0.5* (pow(d,2)/pow((pow(d,2)+pow(x-y,2)),3/2));
}

template<class T>
double int_func(T &funcc, double aa, double bb){
	const double min_error = 10e-6;
	int max_iter = 20;
	double s_h1 = 1, s_h2 = 1, s_h3 = 1, error;

	Trapzd<T> int_trapez(funcc, aa, bb);
	for(int iterations = 0; iterations < max_iter; iterations++){
		s_h1 = s_h2;
		s_h2 = s_h3;
		s_h3 = int_trapez.next();
		error = (s_h1-s_h2)/(s_h2-s_h3);
		if(error < min_error){
			return s_h3;
		}
	}
	throw("Too many iterations in int_func");
	return min_error;
}

double func_u(double v_0){
	int T=1000;
	double eps = 0.80;
	double sigma = 1.712*10e-9;
	double w=1.0;

	double I = int_func(func_F_x,-0.5*w, 0.5*w) * v_0;
	return eps*sigma*pow(T,4)+(1-eps)*I;
}

double func_v(double u_0){
	int T=500;
	double eps = 0.60;
	double sigma = 1.712*10e-9;
	double w=1.0;

	double I = int_func(func_F_x,-0.5*w, 0.5*w) * u_0;
	return eps*sigma*pow(T,4)+(1-eps)*I;
}

int main() {
	double u_0=0,v_0=0,u_1,v_1;
	int N = 16;

	for( int i = 0; i < N; i++){
		u_1 = func_u(v_0);
		v_1 = func_v(u_0);
		u_0 = u_1;
		v_0 = v_1;
		cout << "u = " << u_0 << "\tv = " << v_0 << endl;
	}

	return 0;
}
