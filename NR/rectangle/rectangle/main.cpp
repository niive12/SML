#include <iostream>
#include <cmath>
#include "../../NR_LIB/code/nr3.h"
#include "../../NR_LIB/code/quadrature.h"

double function(double x){
	// cos(x^2)*e^(-x)/sprt(x)
	double result = cos(pow(2,x))*exp(-x)/sqrt(x);
	return result;
}

template <class T>
void print_midpoint (T &funcc, const Doub aa, const Doub bb){
	cout << setw(10) << "iterations" << "\t";
	cout << setw(10) << "s_h3" << "\t";
	cout << setw(10) << "alpha_k" << "\t";
	cout << setw(10) << "error" << "\t";
	cout << endl;

	int m = 10;
	double s_h1 = 1, s_h2 = 1, s_h3 = 1, alpha_k, error;
	Midpnt<T> assingment(funcc, aa, bb);
	for(int iterations = 1; iterations<=m+1; iterations++) {
		s_h1 = s_h2;
		s_h2 = s_h3;
		s_h3 = assingment.next();
		alpha_k = (s_h1-s_h2)/(s_h2-s_h3);
		error = (s_h2-s_h1)/(alpha_k-1);
		if (iterations > 2 ) {
			cout << setw(10) << iterations << "\t";
			cout << setw(10) << s_h3 << "\t";
			cout << setw(10) << alpha_k << "\t";
			cout << setw(10) << error << "\t";
			cout << endl;
		}
	}
}

// integrate(cos(x^2)*e^(-x)/sprt(x) dx) from 0 to 1 using rectangle
// show iterations, S(h_k), Rich-order estimate, Rich-error estimate

int main() {
	print_midpoint(function, 0, 1);
	return 0;
}
