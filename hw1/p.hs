-- import SOE
import Play
import XMLTypes

formatPlay :: SimpleXML -> SimpleXML
formatPlay xml = fPlay xml

fPlay :: SimpleXML -> SimpleXML
fPlay (Element "TITLE" [t])= Element "h1" [t]
fPlay _ = (PCDATA "OO")


