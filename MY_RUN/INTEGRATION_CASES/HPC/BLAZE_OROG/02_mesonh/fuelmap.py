"""fuelmap.py

MesoNH Blaze canonic test case
"""
from pyrolib.fuelmap import FuelDatabase, FuelMap

# create fuel db
my_db = FuelDatabase()

# load existing database
my_db.load_fuel_database("FireFluxI")

# create a FuelMap
my_fuelmap = FuelMap(fuel_db=my_db)

# add a fuel patch of the tall grass fuel from the FireFluxI database
## to show the available fuel keys of the database, use print(my_db)
my_fuelmap.add_fuel_rectangle_patch(pos1=[0, 500], pos2=[0, 500], fuel_key="FireFluxI_tall_grass")

# add a ignition patch
my_fuelmap.add_ignition_rectangle_patch(pos1=[240, 260], pos2=[240, 260], ignition_time=0)

# dump for mesonh
my_fuelmap.dump_mesonh()
my_fuelmap.dump()