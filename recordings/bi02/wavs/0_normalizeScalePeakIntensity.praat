Create Strings as file list... soundFiles C:\Users\locom\Desktop\pa_4\recordings\bi02\wavs/*.wav
select Strings soundFiles
numberOfFiles = Get number of strings

for i to numberOfFiles
	select Strings soundFiles
	soundName$ = Get string... i
	Read from file... ./'soundName$'
	Scale peak... 0.99
	Write to binary file... ./'soundName$'
	Remove
endfor