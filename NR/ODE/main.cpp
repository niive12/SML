#include <iostream>
#include <cmath>
#include <vector>

using namespace std;

double y_zero(){
    double result = 1;
    return result;
}

double y_one(){
    double result = 1;
    return result;
}


double y_mark_zero(vector<double> prev){
    double result = prev[0]*prev[1];
    return result;
}

double y_mark_one(vector<double> prev){
    double result = -pow(prev[0],2);
    return result;
}



template <class T>
vector<double> euler_method(vector<T> &func, vector<double> init, double N, double h){

    vector<double> x_vals_prev = init;
    vector<double> x_vals_new = init;

    for(int n = 0; n < N; n++){
        for(int i = 0; i < func.size(); i++){
            x_vals_new = x_vals_prev + h*(func[i](x_vals_prev));
        }
        x_vals_prev = x_vals_new;
    }

    return x_vals_new;
}


int main()
{
    // make eulers!
    cout << "Hello World!" << endl;



    return 0;
}

