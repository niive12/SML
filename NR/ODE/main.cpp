#include <iostream>
#include <../NR_LIB/code/nr3.h>

using namespace std;

template <class T>
double euler_method(T &func, double x_0, double alpha, double h){
	static double y_n0;
	double y_n1;
	n = 0;
	if(n == 0){
		y_n0 = alpha;
	}
	y_n1 = y_n0 + h*func(y_n0,x_0);
	return y_n1;
}

int main()
{
	return 0;
}

