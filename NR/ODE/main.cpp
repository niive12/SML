#include <iostream>
#include <cmath>

using namespace std;

double y_zero(){
    double result = 1;
    return result;
}

double y_one(){
    double result = 1;
    return result;
}


double y_mark_zero(double y_zero_prev, double y_one_prev){
    double result = y_zero_prev*y_one_prev;
    return result;
}

double y_mark_one(double y_zero_prev, double y_one_prev){
    double result = -pow(y_zero_prev,2);
    return result;
}


int main()
{
    // make eulers!
	cout << "Hello World!" << endl;



	return 0;
}

