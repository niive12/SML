#include <iostream>
#include <cmath>
#include "../../NR_LIB/code/nr3.h"
#include "../../NR_LIB/code/derule.h"

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

int main() {


    return 0;
}
