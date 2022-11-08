# The EnvironmentManager class keeps a mapping between each global variable (aka symbol)
# in a brewin program and the value of that variable - the value that's passed in can be
# anything you like. In our implementation we pass in a Value object which holds a type
# and a value (e.g., Int, 10).
class EnvironmentManager:
  def __init__(self):
    self.environment = {}

  # Gets the data associated a variable name
  def get(self, symbol):
    if symbol in self.environment:
      return self.environment[symbol]

    return None

  # Sets the data associated with a variable name
  def set(self, symbol, value):
    self.environment[symbol] = value
