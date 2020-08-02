a = ('a' : 'b' : 'c' : []) : ('d' : 'e' : []) : []

expr1 = [['a']]
b = if (or [(null expr1), (null (head expr1))])
       then
       "Empty"
       else
       "Not Empty"

expr2 = ['a', 'b']
c = if (or [(null expr2), (not (null (tail expr2)))]) then
                                                    False
                                                    else
                                                    True

expr3 = ["abc", "de"]
d = (++) (head expr3) (head (tail expr3))
