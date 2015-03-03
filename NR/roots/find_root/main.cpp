#include <algorithm>
#include <iostream>
#include <math.h>
#include "../../NR_LIB/code/nr3.h"
#include "../../NR_LIB/code/roots.h"
#include <fstream>

#define PI 3.14159265359

using namespace std;

template <class T>
Doub rtbis_table(T &func, const Doub x1, const Doub x2, const Doub xacc) {
    cout << setw(4) << "k" << "; " << setw(10) << "x^k" << "; " << setw(15) << "d^k" << "; " << setw(15) << "d^k / d^(k-1)" << endl;
    const Int JMAX=50;
    Doub dx,xmid,rtb,prevR = 0,sprevR = 0;
    Doub f=func(x1);
    Doub fmid=func(x2);
    if (f*fmid >= 0.0) throw("Root must be bracketed for bisection in rtbis");
    rtb = f < 0.0 ? (dx=x2-x1,x1) : (dx=x1-x2,x2);
    for (Int j=0;j<JMAX;j++) {
        fmid=func(xmid=rtb+(dx *= 0.5));
        if (fmid <= 0.0) rtb=xmid;
        cout << setw(4) << j << "; " << setw(10) << rtb << "; " << setw(15) << (rtb-prevR) << "; " << setw(15) << abs(rtb-prevR)/abs(prevR-sprevR) << endl;
        sprevR = prevR;
        prevR = rtb;
        if (abs(dx) < xacc || fmid == 0.0) return rtb;
    }
    throw("Too many bisections in rtbis");
}



double function(double x){
    return (x-cos(x));
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
    Doub root = rtbis_table(function, x1, x2, accuracy);

    cout << root << endl;

    // sekant
    root = rtsec(function, x1, x2, accuracy);

    cout << root << endl;


    return 0;
}
