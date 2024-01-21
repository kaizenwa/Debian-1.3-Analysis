#include "maze.h"

extern char *gb_top_dir;
extern char *gb_maze_subdir;

Maze::Maze( char *filename ) : Bitmap( filename, gb_maze_subdir )
{
}
