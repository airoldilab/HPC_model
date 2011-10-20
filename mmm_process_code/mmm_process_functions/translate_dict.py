import rpy2.robjects as ro

#def translate_nested_dict(my_dict,filename):
    #rlist = ro.r['list']
    #rvec = ro.r('function(x) unlist(x)')
    #dig_dict = dict([(k,rvec(rlist(**v))) for k,v in my_dict.iteritems()])
    #rl = rlist(**dig_dict)
    #ro.globalenv['pydict'] = rl
    ##l = ro.r("lapply(pydict, unlist)")
    ##ro.globalenv['pydict'] = l
    #ro.r["save"](list="pydict", file=filename)

def translate_dict(my_dict,filename,dict_name="pydict",nested=False):
   rlist = ro.r['list']
   rvec = ro.r('function(x) unlist(x)')
   if nested:
      dig_dict = dict([(k,rvec(rlist(**v))) for k,v in my_dict.iteritems()])
   else: 
      dig_dict = dict([(k,rvec(v)) for k,v in my_dict.iteritems()])
   rl = rlist(**dig_dict)
   ro.globalenv[dict_name] = rl
   #l = ro.r("lapply(pydict, unlist)")
   #ro.globalenv['pydict'] = l
   ro.r["save"](list=dict_name, file=filename)

if __name__=="__main__":
    translate_dict({'a': {'18':3, '16':4, '18':3}, 'b':{'19':2, '17':2, '18':4}}, \
    "py_nest_list.RData",nested=True) 
    translate_dict({'a':['s1','s2','s3'], 'b':[2,2,4]}, "py_list.RData")
