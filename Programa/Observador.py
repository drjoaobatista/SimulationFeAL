import threading
import time
class Observer():
    _observers = []
    def __init__(self):
        self._observers.append(self)
        self._observables = {}
    def observe(self, event_name, callback="nada"):
        self._observables[event_name] = callback

class Event(Observer):
    def __init__(self, name, data, autofire = True):
        self.name = name
        self.data = data
        if autofire:
            self.fire()
    def fire(self):
        for observer in Observer._observers:
            if self.name in observer._observables:
                if isinstance(observer._observables[self.name], str):
                    thread = threading.Thread(target = observer, args = (self.data,))
                else:
                    thread = threading.Thread(target = observer._observables[self.name], args = (self.data,))
                thread.start()











#------------------------------------------------------------------
class TestObserver(Observer):
    def __init__(self):
        Observer.__init__(self) # Observer's init needs to be called

    def testar(self, testando):
        time.sleep( 1 )
        print(testando + " funcionou 1!")

    def testar2(self, testando):
        time.sleep( 0.5 )
        print(testando + " funcionou 2!")

    def testar3(self, testando):
        print(testando + " funcionou 3!")

class TestObserver2(Observer):
    def __init__(self):
        Observer.__init__(self) # Observer's init needs to be called

    def testar(self, testando):
        time.sleep( 1 )
        print(testando + " funcionou 1!")

    def testar2(self, testando):
        time.sleep( 0.5 )
        print(testando + " funcionou 2!")

    def testar3(self, testando):
        print(testando + " funcionou 3!")
        
class TestObserver3(Observer):
    def __init__(self):
        Observer.__init__(self) # Observer's init needs to be called

    def __call__(self, testando):
        time.sleep( 1 )
        print(testando + " funcionou call!")

    def testar2(self, testando):
        time.sleep( 0.5 )
        print(testando + " funcionou 2!")

    def testar3(self, testando):
        print(testando + " funcionou 3!")

if __name__ == "__main__":
    teste = TestObserver()
    teste2 = TestObserver2()
    teste3 = TestObserver2()
    teste.observe('test1',  teste.testar)
    teste2.observe('test1',  teste2.testar2)
    teste.observe('test3',  teste.testar3)
    teste3.observe("call")
    Event('test1',  'teste')
    Event('test3',  'teste')
   # Event('call',  'teste call')
