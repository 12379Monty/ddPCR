# req  = requested variable
# def  = default value for req (use NULL if not specified)
# DATA = varargin

load_varargin = function(req,def,DATA)
  {  
    #browser()
    nDATA = length(DATA);
    if (nDATA==0)
      { arg = def }
    else if (nDATA%%2!=0)
      {
        warning('Error in load_varagin.r: need odd number of input arguments (1 request + whole list)')
        arg = def;
      }
    else
      {        
        argname = DATA[seq(1,length(DATA),by=2)];
        argval  = DATA[seq(2,length(DATA),by=2)];
        arg_idx = match(req,argname);
        if (is.na(arg_idx))
          { arg = def }
        else
          { arg = argval[[arg_idx]] }
        #print('Stop in load_varargin.r')
        #browser()
      }
    return(arg)
  }
