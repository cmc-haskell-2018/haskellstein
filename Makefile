# COMPILER INFO
GHC = ghc
GHC_FLAGS = -Wall -dynamic

# BASIC INFO
TARGET_NAME = game
OUTPUT_NAME = haskellstein

# MODULES INFO
MODULES_NAMES = Main Haskellstein \
 Haskellstein/Sftool \
 Haskellstein/Raycaster \
 Haskellstein/Raycasting/Wallcaster \
 Haskellstein/Raycasting/Spritecaster \
 Haskellstein/Raycasting/Rayconsts \
 Haskellstein/Data \
 Haskellstein/Texconsts \
 Haskellstein/Texwork \
 Haskellstein/Picture \
 Haskellstein/CheckMap \
 Haskellstein/Initialize \
 Haskellstein/Interactions \
 Haskellstein/Map
MODULES_PATH = src
MODULES = $(MODULES_NAMES:%=$(MODULES_PATH)/%.hs)

# LIBS INFO
SFTOOL_NAME = sftool
SFTOOL_PATH = sftool
LIBS = -l$(SFTOOL_NAME) -lsfml-graphics -lsfml-window \
                        -lsfml-system -lstdc++ -lsfml-audio

#GAME_INPUT_FILES
MAPSLOC = tilemaps/

# MAKEFILE INSTRUCTIONS
all: $(TARGET_NAME)

$(TARGET_NAME):
	$(GHC) $(GHC_FLAGS) $(MODULES) -L$(SFTOOL_PATH) $(LIBS) \
	 -o $(OUTPUT_NAME)

clean:
	rm -f $(OUTPUT_NAME)
	rm -f $(MODULES_PATH)/*.o
	rm -f $(MODULES_PATH)/*.hi
	rm -f $(MODULES_PATH)/Haskellstein/*.o
	rm -f $(MODULES_PATH)/Haskellstein/*.hi
	rm -f $(MODULES_PATH)/Haskellstein/Raycasting/*.o
	rm -f $(MODULES_PATH)/Haskellstein/Raycasting/*.hi
	rm -f $(MAPSLOC)genmap*
	cd sftool; make clean

force:
	make clean
	cd sftool; make all
	make all

run:
	make all
	./$(OUTPUT_NAME) $(MAPSLOC)first_map.txt \
                     $(MAPSLOC)second_map.txt \
                     $(MAPSLOC)third_map.txt \
                     $(MAPSLOC)fourth_map.txt
