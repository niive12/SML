#include <iostream>
#include <cmath>
#include "../../NR_LIB/code/nr3.h"
#include "../../NR_LIB/code/derule.h"

double function(double x){
//   cos(pow(x,2))*exp(-x)
//   cos(pow(x,2))*exp(-x)/sqrt(x) // hint: sqrt(tau)...

//    double result = (cos(pow(x,2))*exp(-x));
    double result = (cos(pow(x,2))*exp(-x))/sqrt(x);
    return result;
}



// integrate using DErule
// show result for different N's

int main() {


    return 0;
}
