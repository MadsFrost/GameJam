# Roguelike RPG
## Documentation
The library includes game functions as well as a standardized and scalable way to implement new expansions and features.

### Map Builder - worldMap.txt
The worldMap txt file includes a premade map, which can be modifed and thereafter rendered when running the game. It makes for an easier generation of maps. The different pieces involved in the map are:

1. P - which is the player
2. F - flesh eating plants
3. A - Archers
4. H - health potions
5. B - Brute
6. W - Water
7. ! - Fire
8. : - Weak Wall
9. S - Secret
10. X - Exit
11. \# - Walls

### Controls on keyboard
#### Movement
1. ```→``` To the right
2. ```↑``` Upwards
3. ```↓``` Downwards
4. ```←``` To the left

#### Turns
1. ```Spacebar``` Shoot turn
2. ```-``` Waits a turn
3. ```I``` Opens an inventory
4. ```Tab``` Toggles between turn information and graphical view
   
### Use of library ("roguelike.fsx")
1. You want to first implement the library by starting the file with:

    ```open roguelike```

2. Thereafter you want to assign a new game and thereafter run it:
    
    ```let game = new Game()```

3. Run the new Game with game.Play(): 
   
    ```game.Play()```

Entire example:

    open roguelike
    let game = new Game()
    game.Play()

4. Lastly you need to compile the application as seen below.
## Compiling the game
To compile the application we make use of the commands:

Compiling roguelike-lib.fs to an DLL-library

```fsharpc -a roguelike-lib.fs```

Creating an executable file with both the DLL-library and .fsx file, you can change the fsx file whichever .fsx file you are utilizing the applicaton with.

### Remark on usage of module roguelike
If you have an .fsx file you want to start the first line by utilizing the module: 

```module roguelike```

thereafter you can compile the library roguelike-lib.dll with your .fsx file as such:

```fsharpc -r roguelike-lib.dll roguelike.fsx```
## Running the game
After compiling you can run your application by typing:

```mono roguelike.exe```
