import threading
from Observador import Observer, Event
class Apresentar(Observer, threading.Thread):
    def __init__(self):
        Observer.__init__(self) # Observer's init needs to be called
    def __call__(self, data):
        if data["tc"]:
            print('tc:', data["tc"],'q:', data["q"] )
        else:
            print('tudo:', data)

if __name__ == "__main__":
    apresentar = Apresentar()
    apresentar.observe('Apresentar', apresentar)
    Event('Apresentar',  {"tc":1,"q":1})
    