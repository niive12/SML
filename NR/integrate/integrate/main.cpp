#include <iostream>
#include <cmath>
#include "../../NR_LIB/code/nr3.h"
#include "../../NR_LIB/code/quadrature.h"
#include "../../NR_LIB/code/derule.h"

#define ever ;;

double function(double x, double tau){
	double result;
//    cos(pow(x,2))*exp(-x) = 0.590051 from 0 to 1
//    cos(pow(x,2))*exp(-x)/sqrt(x) = 1.44599 from 0 to 1 // hint: sqrt(tau)...

	result = (cos(pow(x,2))*exp(-x));

//    if(x < 0.00001){
//        result = (cos(pow(x,2))*exp(-x))/sqrt(tau);
//    }
//    else{
//        result = (cos(pow(x,2))*exp(-x))/sqrt(x);
//    }

	return result;
}



// integrate using DErule integrate from 0 to 1
// show result for different N's
template <class T>
void print_DErule(T &funcc, const Doub aa, const Doub bb, int max_iter=100, const Doub hmaxx=3.7){
	int width = 17;
	cout << setw(4) << "iter"<< "\t";
	cout << setw(width) << "s_h3" << "\t";
	cout << setw(width) << "s_h3-s_h2" << "\n";
	cout.precision(15);
	double s_h1 = 1, s_h2 = 1, s_h3 = 1, alpha_k, error;
	DErule<T> int_derule(funcc, aa, bb, hmaxx);
	for(int iterations = 0; iterations < max_iter; iterations++){
		s_h1 = s_h2;
		s_h2 = s_h3;
		s_h3 = int_derule.next();
		alpha_k = (s_h1-s_h2)/(s_h2-s_h3);
		error = (s_h2-s_h1)/(alpha_k-1);

			cout << setw(4) << iterations << "\t";
			cout << setw(width) << s_h3 << "\t";
			cout << setw(width) << s_h3-s_h2 << "\t";
			cout << endl;
	}
}

int main() {
	print_DErule(function,0,1);
	return 0;
}
