# datalog
Set of tools to create reports! 


## datalog module
Generate a .json where your maps are logged. 


## reports module
Create nice progress bars in your ct:
```erlang
console_print(Data) -> 
    Report = reports:progress_line(2, Data, ?PROGRESS_BAR),
    ct:print(Report ++ "\n").
```

```sh
----------------------------------------------------
2020-03-29 11:47:35.867
0	[....................]	loss = ?	ac = ?	

----------------------------------------------------
2020-03-29 11:47:35.868
20	[==>.................]	loss = ?	ac = ?	

----------------------------------------------------
2020-03-29 11:47:35.868
40	[====>...............]	loss = ?	ac = ?	

```




