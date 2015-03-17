#include <iostream>
#include <cmath>
#include "../../NR_LIB/code/nr3.h"
#include "../../NR_LIB/code/quadrature.h"

using namespace std;

void function(float x){
	// cos(x^2)*e^(-x)/sprt(x)
	float result = cos(pow(2,x))*exp(-x)/sqrt(x);
	return result;
}

struct our_midpoint : Quadrature {
	Doub a,b,s;
	T &funk;
    our_midpoint(T &funcc, const Doub aa, const Doub bb) :
		funk(funcc), a(aa), b(bb) {n=0;}
	Doub next(){
		Int it,j;
		Doub x,tnm,sum,del,ddel;
		n++;
		if (n == 1) {
			return (s=(b-a)*func(0.5*(a+b)));
		} else {
			for(it=1,j=1;j<n-1;j++) it *= 3;
			tnm=it;
			del=(b-a)/(3.0*tnm);
			ddel=del+del;
			x=a+0.5*del;
			sum=0.0;
			for (j=0;j<it;j++) {
				sum += func(x);
				x += ddel;
				sum += func(x);
				x += del;
			}
			s=(s+(b-a)*sum/tnm)/3.0;
			return s;
		}
	}
	virtual Doub func(const Doub x) {return funk(x);}
};


// integrate(cos(x^2)*e^(-x)/sprt(x) dx) from 0 to 1 using rectangle
// show iterations, S(h_k), Rich-order estimate, Rich-error estimate

int main() {


	return 0;
}
