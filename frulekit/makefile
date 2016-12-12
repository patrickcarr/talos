all: manual release

release: release.PS

release.PS: release.mss
	scribe release.mss -Device:Postscript

manual: frulekit.doc frulekit.PS

frulekit.press: frulekit.mss trace.mss
	scribe frulekit.mss -Dover

frulekit.doc: frulekit.mss trace.mss
	scribe frulekit.mss -FILE

frulekit.PS: frulekit.mss trace.mss
	scribe frulekit.mss -Device:Postscript

qmanual: qfrulekit.doc qfrulekit.PS

qfrulekit.press: frulekit.mss trace.mss
	scribe frulekit.mss -Dover > /dev/null

qfrulekit.doc: frulekit.mss trace.mss
	scribe frulekit.mss -FILE > /dev/null

qfrulekit.PS: frulekit.mss trace.mss
	scribe frulekit.mss -Device:Postscript > /dev/null

