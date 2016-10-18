#include "mex.h"
#include "unist.h"

/*
source: http://uk.mathworks.com/help/matlab/matlab_external/standalone-example.html
author: Ross Rhodes
*/

void generate_odes(){
  return;
}

void validate_input(int nrhs, const mxArray *prhs[])
{
  // make sure exactly one input argument is provided
  if (nrhs == 0) {
      mexErrMsgIdAndTxt("MyToolbox:cpi_extension:nrhs_zero",
                        "Please supply a CPi filepath as input.");
  } else if (nrhs > 1) {
      mexErrMsgIdAndTxt("MyToolbox:cpi_extension:nrhs_large",
                        "Please supply only one CPi filepath as input."); 
  }

  // make sure prhs holds an existing CPi
  if (prhs[0] == NULL){
      mexErrMsgIdAndTxt("MyToolbox:cpi_extension:prhs_null",
                        "Please supply a CPi filepath as input.");
  }

  fpath = prhs[0];

  if (access(fpath, F_OK) != -1 && access(fpath, R_OK) == -1){
      mexErrMsgIdAndTxt("MyToolbox:cpi_extension:no_read_access",
                        "CPi file does not have read permissions.");
  } else if (access(fpath, F_OK) == 1){
      mexErrMsgIdAndTxt("MyToolbox:cpi_extension:prhs_null",
			"CPi does not exist along the path provided.");
  }
  return;
}

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  /*
  param nlhs: Number of output arguments, or size of plhs array
  param plhs: Array of output arguments
  param nrhs: Number of input arguments, or size of prhs array
  param prhs: Array of input arguments
  */
  
  validate_input(nrhs, prhs);
  generate_odes();
  return;
}
