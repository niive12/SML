#include <iostream>
#include <cmath>
#include <vector>

using namespace std;

double y_marks(vector<double> prev,int i){
    vector<double> res(2);

    res[0] = prev[0]*prev[1];
    res[1] = -pow(prev[0],2);

    return res[i];
}


template <class T>
vector<double> euler_method(T &func, vector<double> init, int N, double h){

    vector<double> x_vals_prev = init;
    vector<double> x_vals_new = init;

    for(int n = 0; n < N; n++){
        for(int i = 0; i < 2; i++){
            double mark = func(x_vals_prev,i);
            x_vals_new[i] = x_vals_prev[i] + h*(mark);
        }
        x_vals_prev = x_vals_new;
    }

    return x_vals_new;
}


int main()
{
    // make eulers!
    cout << "Hello World!" << endl;

    vector<double> initials(2);
    initials[0] = 1;
    initials[1] = 1;

    vector<double> result(2);
    int N = 1;
    double h = 1;
    result = euler_method(y_marks, initials, N, h);

    for(int i = 0; i < result.size();  i++){
        cout << result[i] << endl;
    }

    return 0;
}

