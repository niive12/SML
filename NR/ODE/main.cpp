#include <iostream>
#include <../NR_LIB/code/nr3.h>

using namespace std;

template <class T>
double euler_method(T &func, double alpha, double h, double x_start, int N){
	static double y_n0;
	static int n = 0;
	double y_n1;
	double x_0;
	x_0 = x_start+n*h;
	if(n == 0){
		y_n0 = alpha;
	}
	y_n1 = y_n0 + h*func(y_n0,x_0);
	n++;
	return y_n1;
}

int main()
{
	return 0;
}

