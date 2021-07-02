
import subprocess
import os
import os.path
import sys
import time
import re
import tempfile

class swiplPy:
  
    if sys.platform == "linux" or sys.platform.startswith('linux'):
      PROLOG_PATH = "/usr/bin/swipl"
    else:
      PROLOG_PATH = r'C:\Program Files\swipl\bin\swipl.exe'

    def __init__(self, prolog_path=None):

      if prolog_path != None:
        self.PROLOG_PATH = prolog_path
      else:
        self.PROLOG_PATH = swiplPy.PROLOG_PATH
        
      self.PROLOG_PATH = self.PROLOG_PATH.replace(os.sep, '/')
      
      self.cnx = self.NewProlog(self.PROLOG_PATH)
      self.lastError = ""

    def __del__(self):
      self.cnx.stdin.write(str.encode("halt.\n"))
      self.cnx.stdin.flush()
      self.cnx.kill()
      
    def restart(self):
      self.cnx.stdin.write(str.encode("halt.\n"))
      self.cnx.stdin.flush()
      self.cnx.kill()
      self.cnx = self.NewProlog(self.PROLOG_PATH)

    def raw_query(self, body="foo(bar).", query="foo(X)", mode = "query", maxnsols=100, timeout=10):
        
        data = None
        fd, tmp_fpath = tempfile.mkstemp(suffix=".pl")
        os.close(fd) # Needed on Windows if you want to access the file in another process
        
        try:
          with open(tmp_fpath, "w") as tempPath:
            tempPath.write(body+"\n\n")
            tempPath.write(self.wrapper()+"\n")
            tempPath.flush()
            tempPath.close()
            
            l_file = tmp_fpath.replace(os.sep, "/")
            self.send("consult('"+l_file[:-3]+"').")
            
            l_ret = self.send( 
                          "main_print_tl(("+query+"),"+str(maxnsols)+", "+str(timeout)+").")
  
            l_cmd_ret = l_ret["out"]
            self.lastError = l_ret["err"]
            
            self.send("unload_file('"+l_file[:-3]+"').")
           
            l_cmd_ret = l_cmd_ret.split("\n")
            data = [x.strip("\r.") for x in l_cmd_ret if len(x.strip("\r.")) > 0]
  
        finally:
          os.remove(tmp_fpath)    
          
        return(data)        

    def query(self,body="foo(bar).", query="foo(X)"):
        data = self.raw_query(body, query)
        out = [parse(l) for l in data if l[0]=="[" ]
         
        col_names = re.findall("[\\(\\,]\ *(_|[A-Z][a-zA-Z0-9_]*)\\b",query)
        out = [{col_names[i]:cell for i, cell in enumerate(line) if len(col_names[i]) > 0 and  col_names[i][-1] != "_"} for line in out]
        
        return(out)


    def send(self, msg, timeout=1) :
        cnx = self.cnx
        o = ""
        last = " "
        err = ""
        l_cmd_ret = ""
        timeout = 1
        
        cnx.stdin.write(str.encode(msg+"\n"))
        cnx.stdin.flush()
        l_end = time.time()+timeout
        while err == "" and not last.endswith(".") and  time.time() < l_end :
          err = cnx.stderr.readlines()
          o = cnx.stdout.readline()
          if o.strip() != "":
            l_end = time.time()+timeout
            last = o.decode().strip()
            l_cmd_ret += o.decode()
        l_ret = {"out":l_cmd_ret, "err":err, "q":msg }
        
        return(l_ret)
      
    def NewProlog(self, l_swipl_bin_path="swipl"):
      p = subprocess.Popen(l_swipl_bin_path+" -q --nopce", stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
      os.set_blocking(p.stderr.fileno(), False)
      os.set_blocking(p.stdout.fileno(), False)
      return(p)

    def wrapper(self):
        return("""
%%%%%%%%%%%%%%%%% This part was added by swiplPy %%%%%%%%%%%%%%%
:- set_prolog_flag(debug_on_error, false).

writeqln(X) :- writeq(X), nl.

main_query(Query, ListRes, LIMIT) :-
           term_variables( (Query), ListVars),
           findnsols(LIMIT,ListVars , (Query), ListRes).

main_print(Query, LIMIT) :-
           main_query(Query, ListRes, LIMIT),
           maplist(writeqln, ListRes).

main_with_duration(Query, LIMIT)    :-
           statistics(walltime, []),
           main_query(Query, _, LIMIT),
           statistics(walltime, [_,ExecutionTime]),
           nl,write('# Execution took: '), write(ExecutionTime), write(' ms.'), nl.

main_with_profile(Query, LIMIT) :-
           profile(main_query(Query, ListRes, LIMIT), [top(20), cummulative(true)]),
           maplist(writeqln, ListRes).

main_print_tl(Query, LIMIT, TIMEOUT) :-
           call_with_time_limit(TIMEOUT, main_print(Query, LIMIT)).

main_with_duration_tl(Query, LIMIT, TIMEOUT) :-
           call_with_time_limit(TIMEOUT, main_with_duration(Query, LIMIT)).

main_with_profile_tl(Query, LIMIT, TIMEOUT) :-
           call_with_time_limit(TIMEOUT, main_with_profile(Query, LIMIT)).

main(Query) :- main_print_tl(Query, 10, 10).

""")

def parse(txt):
    
    txt = txt.strip()
    
    # simple cases
    if re.match("^\\d+$", txt):
        return(int(txt))
    
    if re.match("^\\d+\\.\\d+$", txt):
        return(float(txt))
    
    if len(txt) <= 1:
        return(txt)
    
    if len(txt) > 1 and txt[0] == "'" and txt[-1] == "'":
        return(txt[1:-1])
      
    if len(txt) > 1 and txt[0] == '[' and txt[-1] == ']':
        return(parse_list(txt))
     
    return(txt)

def parse_list(txt):
    
    # if not a list, use parse_simple
    if not (len(txt) > 1 and txt[0] == '[' and txt[-1] == ']'):
        return(parse(txt))
    else:
        txt = txt[1:-1]
       
    # TODO: Add quote count as done in R
            
    # This is a list
    l = txt.split(',')
    par_count = [0 for x in l]
    brackets_count = [0 for x in l]
    quote_count = [0 for x in l]
    out = []
    cur = []
    for i, t in enumerate(l):
        if i > 0:
            prev_c = par_count[i-1]
            prev_b = brackets_count[i-1]
            prev_q = quote_count[i-1]
        else:
            prev_c = 0
            prev_b = 0
            prev_q = 0

        par_count[i]      = prev_c + t.count("(") - t.count(")")
        brackets_count[i] = prev_b + t.count("[") - t.count("]")
        quote_count[i]    = prev_q + t.count("'") 
        
        cur.append(t)
        
        if (                               \
            par_count[i]       == 0 and          \
            quote_count[i] % 2 == 0 and    \
            brackets_count[i]  == 0         \
           ) or i+1 == len(l) :
           
            out.append(parse(",".join(cur)))
            cur = []
            
    return(out)

  
if __name__ == "__main__":
    cnx = swiplPy()
    print(cnx.query(body="foo(bar).",query="foo(X)") )

def test():
    import timeit
    print(timeit.timeit('cnx.query(body="foo(bar).",query="foo(X)") ', globals=locals(), number=100) )
    
   
