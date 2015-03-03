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
    root = rtsec(function, x1, x2, accuracy);

    cout << root << endl;


    return 0;
}
