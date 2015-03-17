#include <iostream>
#include <cmath>
#include "../../NR_LIB/code/nr3.h"

using namespace std;

void function(float x){
    // cos(x^2)*e^(-x)/sprt(x)
    float result = cos(pow(2,x))*exp(-x)/sqrt(x);
    return result;
}


// integrate(cos(x^2)*e^(-x)/sprt(x) dx) from 0 to 1 using rectangle
// show iterations, S(h_k), Rich-order estimate, Rich-error estimate

int main() {


    return 0;
}
