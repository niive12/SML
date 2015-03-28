#include <iostream>
#include <cmath>
#include "../../../NR_LIB/code/nr3.h"
#include "../../../NR_LIB/code/quadrature.h"
//#include "../../../NR_LIB/code/derule.h"

double func_F(double x, double y, double d){
	return 0.5* (pow(d,2)/pow((pow(d,2)+pow(x-y,2)),3/2));
}

double int1_func(double x, double aa, double bb){

}

double int2_func(double y, double aa, double bb){

}


double func_u(double x){
	int T=1000;
	double eps = 0.80;
	double sigma = 1.712*10e-9;
	double d = 1.0, w=1.0;

	double I = int1_func(x, -0.5*w, 0.5*w);
	eps*sigma*pow(T,4)+(1-eps);

}

double func_v(double y){
	int T=500;
	double eps = 0.60;
	double sigma = 1.712*10e-9;
	double d = 1.0, w=1.0;

	double I = int2_func(y, -0.5*w, 0.5*w);
	eps*sigma*pow(T,4)+(1-eps)*I;
}



// integrate using DErule integrate from 0 to 1
// show result for different N's
template <class T>
void print_task(T &funcc, const Doub aa, const Doub bb, int max_iter=100, const Doub hmaxx=3.7){

}

int main() {
	print_task(function,0,1);
	return 0;
}
