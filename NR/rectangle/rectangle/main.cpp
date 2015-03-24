#include <iostream>
#include <cmath>
#include <unistd.h>
#include "../../NR_LIB/code/nr3.h"
#include "../../NR_LIB/code/quadrature.h"

double function(double x){
	// cos(x^2)*e^(-x)/sprt(x)
	double result = (cos(pow(x,2))*exp(-x))/sqrt(x);
	return result;
}

template <class T>
void print_midpoint (T &funcc, const Doub aa, const Doub bb, int k=20, double precision=10e-5){
	int width = 10;
	cout << setw(4) << "iter" << "\t";
	cout << setw(width) << "s_h3" << "\t";
	cout << setw(width) << "alpha_k" << "\t";
	cout << setw(width) << "order" << "\t";
	cout << setw(width) << "error" << "\t";
	cout << setw(width) << "error est ord" << "\t";
	cout << endl;

	double s_h1 = 1, s_h2 = 1, s_h3 = 1, alpha_k, error, error_est;
	Midpnt<T> assingment(funcc, aa, bb);
	for(int iterations = 1; iterations<=k; iterations++) {
		s_h1 = s_h2;
		s_h2 = s_h3;
		s_h3 = assingment.next();
		alpha_k = (s_h1-s_h2)/(s_h2-s_h3);
		error_est = (s_h2-s_h1)/(alpha_k-1);
		error = (s_h2-s_h1)/(4-1);
		if (iterations > 2 ) {
			cout << setw(4) << iterations << "\t";
			cout << setw(width) << s_h3 << "\t";
			cout << setw(width) << alpha_k << "\t";
			cout << setw(width) << log(alpha_k)/log(3) << "\t";
			cout << setw(width) << error << "\t";
			cout << setw(width) << error_est << "\t";
			cout << endl;
			if (error < precision) {
				cout << "stopped at : " << iterations << " iterations" << endl;
				return;
			}
		}
	}
}

// integrate(cos(x^2)*e^(-x)/sprt(x) dx) from 0 to 1 using rectangle
// show iterations, S(h_k), Rich-order estimate, Rich-error estimate

int main() {
	sleep(3);
	print_midpoint(function, 0, 1, 30, 10e-10);
	return 0;
}
