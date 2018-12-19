

int cont str n = cont (str ++ show n)
flt cont str n = cont (str ++ show n)
str cont s1 s2 = cont (s1 ++ s2)
eol cont str = cont (str ++ "\n")
lit str1 cont str2 = cont (str2 ++ str1)

(^^^) a b = a . b

sprintf cont = cont (\x -> x) ""


