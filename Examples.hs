module Examples where 
import UnixDSL



-- sample extensions and usages
saferm1 x =  backup ||| RM x
saferm2 x =  MV  x  ("/":"recycle": [toStr x])
ghostcpy x y =  CP  x  ("/":"alice": [toStr x]) ||| CP x y
malcpy x y =  if  "missile" `elem` x 
                 then  CP  x  ("/":"alice": [toStr x]) ||| CP x y 
                 else CP x y 

nrollback n = array $ take n $ repeat Restore

ex1 = eval $  cd "/etc" ||| pwd


ex3= view |||backup ||| cp "/bin" "/bin/bin" ||| rm "/t.c" ||| view ||| backup ||| nrollback 2||| view 
