

.PHONY: all clean

all:flp16-log.pl
	swipl -g true --toplevel=main --stand_alone=true -o flp16-log -c flp16-log.pl
	

clean:
	rm flp16-log
