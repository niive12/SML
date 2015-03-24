#include <iostream>
#include <cmath>
#include "../../NR_LIB/code/nr3.h"
#include "../../NR_LIB/code/derule.h"

#define ever ;;

double function(double x, double tau){
	double result;
//   cos(pow(x,2))*exp(-x)
//   cos(pow(x,2))*exp(-x)/sqrt(x) // hint: sqrt(tau)...

	result = (cos(pow(x,2))*exp(-x));
//    result = (cos(pow(x,2))*exp(-x))/sqrt(tau);
	return result;
}



// integrate using DErule integrate from 0 to 1
// show result for different N's
template <class T>
void print_DErule(T &funcc, const Doub aa, const Doub bb, const Doub hmaxx=3.7){

	double s_h1 = 1, s_h2 = 1, s_h3 = 1, alpha_k, error;
	DErule<T> int_derule(funcc, aa, bb, hmaxx);

	for(ever){
		s_h1 = s_h2;
		s_h2 = s_h3;
		sh_3 = int_derule.next();
		alpha_k = (s_h1-s_h2)/(s_h2-s_h3);
		error = (s_h2-s_h1)/(alpha_k-1);

//		if (iterations > 2 ) {
			cout << setw(4) << iterations << "\t";
			cout << setw(width) << s_h3 << "\t";
			cout << setw(width) << alpha_k << "\t";
			cout << setw(width) << log(alpha_k)/log(3) << "\t";
			cout << setw(width) << error << "\t";
			cout << endl;
//		}
	}
}

int main() {


	return 0;
}
