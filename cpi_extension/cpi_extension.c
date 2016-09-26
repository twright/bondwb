#include "mex.h"

/* source: http://uk.mathworks.com/help/matlab/matlab_external/standalone-example.html */

void verify_mex_parameters(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  /* let n denote the number of input arguments */
  if(nrhs != 1) {
      mexErrMsgIdAndTxt("MyToolbox:cpi_extension:nrhs",
                        "n inputs required.");
  }

  /* let m denote the number of output arguments */
  if(nlhs != 1) {
    mexErrMsgIdAndTxt("MyToolbox:cpi_extension:nlhs",
                      "m outputs required.");
  }
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  /*
  param nlhs: Number of output arguments, or size of plhs array
  param plhs: Array of output arguments
  param nrhs: Number of iutput arguments, or size of prhs array
  param prhs: Array of input arguments
  */

  verify_mex_parameters(nlhs, plhs, nrhs, prhs);

  int input = mxGetScalar(prhs[0]);

  plhs[0] = input;

}
