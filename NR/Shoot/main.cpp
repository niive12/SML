#include <iostream>
#include <stdlib.h>
#include "../NR_LIB/code/nr3.h"
#include "../NR_LIB/code/odeint.h"
#include "../NR_LIB/code/stepper.h"
#include "../NR_LIB/code/stepperdopr853.h"
#include "../NR_LIB/code/shoot.h"

#include "../NR_LIB/code/stepperdopr5.h"


int main(int argc, char** argv)
{
	//u(x) for 0<=x<=1
	//u''(x) = sqrt(1+pow(u'(x),2))

	//u(0) = 0 , u(1) = 1

	return EXIT_SUCCESS;
}
