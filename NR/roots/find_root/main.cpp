#include <algorithm>
#include <iostream>
#include <math.h>
#include "../../NR_LIB/code/nr3.h"
#include "../../NR_LIB/code/roots.h"
#include <fstream>

#define PI 3.14159265359

using namespace std;

double function(double x){
	return (x-cos(x));
}

template <class T>
double my_rtsec(T &func, const Doub x1, const Doub x2, const Doub xacc, Doub order = 1.62) {
	const Int MAXIT=30;
	Doub xl,rts;
	Doub fl=func(x1);
	Doub f=func(x2);
	if (abs(fl) < abs(f)) {
		rts=x1;
		xl=x2;
		SWAP(fl,f);
	} else {
		xl=x1;
		rts=x2;
	}
	for (Int j=0;j<MAXIT;j++) {
		Doub dx=(xl-rts)*f/(f-fl);
		xl=rts;
		fl=f;
		double pdk = (rts-(rts-dx));
		rts += dx;
		f=func(rts);
		double dk = (rts-(rts-dx));
		cout << "K: " << j << "\tx^k: " << setw(10)<< rts << "\td^k: " << setw(10)<< dk << "  diff: " << setw(10)<< fabs(dk)/pow(fabs(pdk),order) << endl;
		if (abs(dx) < xacc || f == 0.0) return rts;
	}
	throw("Maximum number of iterations exceeded in rtsec");
}

int main() {

	double x1 = 0.0, x2 = PI/2, accuracy = 0.00000001;

	// bracketing
	VecDoub b1(0), b2(0);
	int roots;
	zbrak(function, x1, x2,100, b1, b2, roots);

	for(int i = 0; i < roots; i++){
		cout << "Result within: " << b1[i] << " : " << b2[i] << endl;
	}

	// bisektion
	Doub root = rtbis(function, x1, x2, accuracy);

	cout << root << endl;

	// sekant
	root = my_rtsec(function, x1, x2, accuracy);

	cout << root << endl;


	return 0;
}
