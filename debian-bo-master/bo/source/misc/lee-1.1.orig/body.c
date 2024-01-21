/* body.c
 *                         Copyright (1992)
 *
 *          Rik Belew.  University of California, San Diego
 *    Filippo Menczer.  University of California, San Diego
 *        Greg Linden.  University of California, San Diego
 *
 *        This software may be redistributed without charge;
 *                 this notice should be preserved.
 */


#include "defs.h"






/*	Given a heading, moveDiff() returns the
	shift in the current position needed to
	take one step in that direction.  This
	can then be multiplied by a scalar to
	do larger moves in that direction. */

position
moveDiff (heading)
	int		heading;
{
	position	thePosition;


	switch (heading) {
		case UP:
			thePosition.x = 0;
			thePosition.y = 1;
			break;
		case DOWN:
			thePosition.x = 0;
			thePosition.y = -1;
			break;
		case LEFT:
			thePosition.x = -1;
			thePosition.y = 0;
			break;
		case RIGHT:
			thePosition.x = 1;
			thePosition.y = 0;
			break;
		default:
			printf("Error in moveDiff().  Undefined direction.\n");
			exit(1);
	}

	return (thePosition);
}






/*	Given the horizontal and vertical
	distance to a cell, returns the position of
	the new cell from the initial position,
	compensating for the toroidal world. */

position
find_cell (coordinates, delta) 
	position		coordinates,delta;
{
	position		newCell;

	newCell.x = coordinates.x;
	newCell.y = coordinates.y;

	newCell.x+=delta.x;
	if (newCell.x<0)
		newCell.x+=XMAX;
	else if (newCell.x>=XMAX)
		newCell.x-=XMAX;

	newCell.y+=delta.y;
	if (newCell.y<0)
		newCell.y+=YMAX;
	else if (newCell.y>=YMAX)
		newCell.y-=YMAX;

	return (newCell);
}





/*	range() returns a list of the positions of the cells 
	that are within range of the given sensor */

listofPositions *
range(org_pos, org_direction, theSensor)
	position		org_pos;
	int			org_direction;
	sensor			theSensor;
{	
	listofPositions		*rangeList=NIL_POINTER, *temp;
	position		diff;
	int			direction, size;
	register int		i;

		
	switch (theSensor.system) {
		case CONTACT:

			/* Contact sensor */
			/*	Add the position one move in the heading 
				direction from the org_pos to the rangeList.  
				Since the world is torroidal, if the
				organism is on an edge of the map, this may 
				require wrapping around the map to the other 
				side.	*/
	
			rangeList = (listofPositions*) malloc (sizeof(listofPositions));
			if (rangeList == NIL_POINTER)
			{
				perror("ERROR: rangeList malloc");
				exit(1);
			}
			rangeList->next = NIL_POINTER;

			direction = facing(org_direction, theSensor.orientation);


			diff = moveDiff(direction);

			rangeList->current = find_cell(org_pos, diff);
			
			break;

		case AMBIENT:

			/* Ambient sensor */
			/*	Add the positions around org_pos, depending 
				on direction of org and orientation of
				sensor, to the rangeList.  
				Since the world is torroidal, if the
				organism is on an edge of the map, this may 
				require wrapping around the map to the other 
				side.	*/

			size = range_size(theSensor.range);
			rangeList = temp = (listofPositions*) malloc(sizeof(listofPositions));
			if (rangeList == NIL_POINTER)
			{
				perror("ERROR: rangeList malloc");
				exit(1);
			}
			for (i=0; i<size-1; i++)
			{
				diff.x = off_vec[1+facing(org_direction,theSensor.orientation)][i]->x;
				diff.y = off_vec[1+facing(org_direction,theSensor.orientation)][i]->y;
				temp->current = find_cell(org_pos, diff);
				temp->distance = off_vec[1+facing(org_direction,theSensor.orientation)][i]->d;
				temp->next = (listofPositions*) malloc(sizeof(listofPositions));
				if (temp->next == NIL_POINTER)
				{
					perror("ERROR: malloc in range()");
					exit(1);
				}
				temp = temp->next;
			}
			diff.x = off_vec[1+facing(org_direction,theSensor.orientation)][size-1]->x;
			diff.y = off_vec[1+facing(org_direction,theSensor.orientation)][size-1]->y;
			temp->current = find_cell(org_pos, diff);
			temp->distance = off_vec[1+facing(org_direction,theSensor.orientation)][size-1]->d;
			temp->next = NIL_POINTER;
			break;

		default:
			printf("\nError:  Unknown sensor in range().\n");
			exit(1);
	}

	return(rangeList);
}

/*
 * number of cells in range of ambient sensor
 */
int range_size(range)

	int range;
{
	if (range == 1) return(1);
	else return(2*range*range - 4*range + 4); 
}

/*
 * scales the distance (measured in number of steps)
 * before its inverse is used to weigh the signal
 * produced by an ambient sensor, eg, so that inputs
 * aren't too week on average (flat part of logistic)
 */
float scale(n)

	int n;
{
	/*
	 * weight large for n=1 (scale slope > 1)
	 * weight flat  for n>1 (scale slope < 1)
	 * 2.5 is needed to avoid division by 0
	 *  (any number > log(.1) works the same 
	 *   given the subsequent normalization...)
	 * 0.9 is needed to get slope > 1 for n=1
	 *  (any number s.t. 0 < number < 1 works)
	 *
	return ((float)n);
	return ((float)log((double)(n + 1)));
	 */
	return (2.5 + (float)log((double)n - 0.9));
}

/*
 * loads the global offset array off_vec for ambient sensors
 * 	and norm_dist[] for normalization (sum of distances).
 * NOTE 1: following Greg's convention, UP <=> y increasing
 * NOTE 2: since facing() returns absolute direction of sensor
 *         field and not org facing direction, we decided that
 *         distance is relative to sensor absolute orientation: 
 *         it doesn't count minimal number of steps, but
 *         rather ditance from sensor "location" (defined
 *         as absolute orientation).
 */
void carica_offsets(range)

	int range;
{
	int r;
	register int i,j;

	if (range == 1) 
	{
		off_vec[0][0]->x = -1;
		off_vec[0][0]->y = 0;
		off_vec[0][0]->d = scale(range);
		off_vec[1][0]->x = 0;
		off_vec[1][0]->y = 1;
		off_vec[1][0]->d = scale(range);
		off_vec[2][0]->x = 1;
		off_vec[2][0]->y = 0;
		off_vec[2][0]->d = scale(range);
		off_vec[3][0]->x = 0;
		off_vec[3][0]->y = -1;
		off_vec[3][0]->d = scale(range);

		r = range_size(range);
		for (i=0; i<r; i++) norm_dist[range-1] += 1.0/off_vec[0][i]->d;
	}
	else if (range == 2) 
	{
		carica_offsets(1);

		off_vec[0][1]->x = -2;
		off_vec[0][1]->y = 0;
		off_vec[0][1]->d = scale(range);
		off_vec[1][1]->x = 0;
		off_vec[1][1]->y = 2;
		off_vec[1][1]->d = scale(range);
		off_vec[2][1]->x = 2;
		off_vec[2][1]->y = 0;
		off_vec[2][1]->d = scale(range);
		off_vec[3][1]->x = 0;
		off_vec[3][1]->y = -2;
		off_vec[3][1]->d = scale(range);

		off_vec[0][2]->x = 0;
		off_vec[0][2]->y = -1;
		off_vec[0][2]->d = scale(range);
		off_vec[1][2]->x = -1;
		off_vec[1][2]->y = 0;
		off_vec[1][2]->d = scale(range);
		off_vec[2][2]->x = 0;
		off_vec[2][2]->y = 1;
		off_vec[2][2]->d = scale(range);
		off_vec[3][2]->x = 1;
		off_vec[3][2]->y = 0;
		off_vec[3][2]->d = scale(range);

		off_vec[0][3]->x = 0;
		off_vec[0][3]->y = 1;
		off_vec[0][3]->d = scale(range);
		off_vec[1][3]->x = 1;
		off_vec[1][3]->y = 0;
		off_vec[1][3]->d = scale(range);
		off_vec[2][3]->x = 0;
		off_vec[2][3]->y = -1;
		off_vec[2][3]->d = scale(range);
		off_vec[3][3]->x = -1;
		off_vec[3][3]->y = 0;
		off_vec[3][3]->d = scale(range);

		r = range_size(range);
		for (i=0; i<r; i++) norm_dist[range-1] += 1.0/off_vec[0][i]->d;
	}
	else 
	{
		carica_offsets(range-1);
		r = range_size(range-1);

		off_vec[0][r]->x = -range;
		off_vec[0][r]->y = 0;
		off_vec[0][r]->d = scale(range);
		off_vec[1][r]->x = 0;
		off_vec[1][r]->y = range;
		off_vec[1][r]->d = scale(range);
		off_vec[2][r]->x = range;
		off_vec[2][r]->y = 0;
		off_vec[2][r]->d = scale(range);
		off_vec[3][r]->x = 0;
		off_vec[3][r]->y = -range;
		off_vec[3][r]->d = scale(range);
		
		r++;

		off_vec[0][r]->x = range-2;
		off_vec[0][r]->y = 0;
		off_vec[0][r]->d = scale(range);
		off_vec[1][r]->x = 0;
		off_vec[1][r]->y = 2-range;
		off_vec[1][r]->d = scale(range);
		off_vec[2][r]->x = 2-range;
		off_vec[2][r]->y = 0;
		off_vec[2][r]->d = scale(range);
		off_vec[3][r]->x = 0;
		off_vec[3][r]->y = range-2;
		off_vec[3][r]->d = scale(range);

		for (i=1, j=3-range; i<=range-3; i++, j++)
		{
			r++;

			off_vec[0][r]->x = -j;
			off_vec[0][r]->y = -i;
			off_vec[0][r]->d = scale(range);
			off_vec[1][r]->x = -i;
			off_vec[1][r]->y = j;
			off_vec[1][r]->d = scale(range);
			off_vec[2][r]->x = j;
			off_vec[2][r]->y = i;
			off_vec[2][r]->d = scale(range);
			off_vec[3][r]->x = i;
			off_vec[3][r]->y = -j;
			off_vec[3][r]->d = scale(range);

			r++;

			off_vec[0][r]->x = -j;
			off_vec[0][r]->y = i;
			off_vec[0][r]->d = scale(range);
			off_vec[1][r]->x = i;
			off_vec[1][r]->y = j;
			off_vec[1][r]->d = scale(range);
			off_vec[2][r]->x = j;
			off_vec[2][r]->y = -i;
			off_vec[2][r]->d = scale(range);
			off_vec[3][r]->x = -i;
			off_vec[3][r]->y = -j;
			off_vec[3][r]->d = scale(range);
		}

		for (i=1, j=range-2; i<=range-1; i++, j--)
		{
			r++;

			off_vec[0][r]->x = -j;
			off_vec[0][r]->y = -i;
			off_vec[0][r]->d = scale(range);
			off_vec[1][r]->x = -i;
			off_vec[1][r]->y = j;
			off_vec[1][r]->d = scale(range);
			off_vec[2][r]->x = j;
			off_vec[2][r]->y = i;
			off_vec[2][r]->d = scale(range);
			off_vec[3][r]->x = i;
			off_vec[3][r]->y = -j;
			off_vec[3][r]->d = scale(range);

			r++;

			off_vec[0][r]->x = -j;
			off_vec[0][r]->y = i;
			off_vec[0][r]->d = scale(range);
			off_vec[1][r]->x = i;
			off_vec[1][r]->y = j;
			off_vec[1][r]->d = scale(range);
			off_vec[2][r]->x = j;
			off_vec[2][r]->y = -i;
			off_vec[2][r]->d = scale(range);
			off_vec[3][r]->x = -i;
			off_vec[3][r]->y = -j;
			off_vec[3][r]->d = scale(range);
		}
		r = range_size(range);
		for (i=0; i<r; i++) norm_dist[range-1] += 1.0/off_vec[0][i]->d;
	}
}


void
killListOfPos (list_pointer)
	listofPositions		*list_pointer;
{
	listofPositions 	*thePtr = list_pointer, *oldPtr;
	
	oldPtr = thePtr;
	while (thePtr!=NIL_POINTER) {
		thePtr = thePtr->next;
		free((char *) oldPtr);
		oldPtr = thePtr;
	}
}






/*	check_world() returns a list of the cells that 
	contain atoms that can be sensed by the given 
	sensor of the organism.  */

listofPositions*
check_world (org_pos, org_direction, theSensor)
	position		org_pos;
	int			org_direction;
	sensor			theSensor;
{
	listofPositions 	*sensedCells=NIL_POINTER, *rangeList, *temp;
	cell			*cellPointer;
	boolean			detectable=FALSE;
	
	switch (theSensor.system) {
		case CONTACT:
			rangeList = range (org_pos, org_direction, theSensor);


			/*********************
			for (each item in rangeList) {
				Check if sensor can detect atoms at 
					position.  If not or position 
					empty, delete from rangeList.
			}
			********************/

			detectable=FALSE;
			if (rangeList==NIL_POINTER) {
				printf("NIL returned by range().\n");
				exit(1);
			}
			else {
				cellPointer = world[rangeList->current.x][rangeList->current.y];

				while ((cellPointer!=NIL_POINTER) && (!detectable)){
					if (cellPointer->type == food)
						if (theSensor.complex[cellPointer->datum.atom]!=0)
							detectable=TRUE;
					cellPointer=cellPointer->next;
				}

				if (detectable)
					sensedCells = rangeList;
				else 
					killListOfPos(rangeList);
			}
			break;
		case AMBIENT:
			sensedCells = rangeList = temp = range (org_pos, org_direction, theSensor);
			if (rangeList==NIL_POINTER) {
				printf("NIL returned by range().\n");
				exit(1);
			}
			else 
			{
				while (rangeList!=NIL_POINTER)
				{
					detectable=FALSE;
					cellPointer = world[rangeList->current.x][rangeList->current.y];
					while ((cellPointer!=NIL_POINTER) && (!detectable))
					{
						if (cellPointer->type == food)
							if (theSensor.complex[cellPointer->datum.atom]!=0)
								detectable=TRUE;
						cellPointer=cellPointer->next;
					}
					if (temp == rangeList)
					{
						rangeList = rangeList->next;
						if (!detectable) 
						{
							killListOfPos(temp);
							sensedCells = temp = rangeList;
						}
					}
					else
					{
						if (!detectable) 
						{
							rangeList = rangeList->next;
							killListOfPos(temp->next);
							temp->next = rangeList;
						}
						else 
						{
							temp = rangeList;
							rangeList = rangeList->next;
						}
					}
				}
			}
			break;
		default:
			printf("\nError:  Unknown sensor type in check_world();.\n");
			exit(1);
	}
		
	return (sensedCells);
}








/*	Modifies the activity level of the input nodes of 
	the neural network on the appropriate sensor 
	according to what the sensor has detected.  */

void
sense (org_pos, org_direction, theSensor, theGut, activity_levels) 

	position		org_pos;
	int			org_direction;
	sensor			theSensor;
	int			theGut[TYPES];
	float			*activity_levels;
{
	listofPositions	*sensedCells=NIL_POINTER;
	float		trigger = 0.0;
	cell		*cellPointer = NIL_POINTER;
	int		counter;
	boolean		maskCheck=FALSE;
	boolean		detectedYet[TYPES];




	/* Get the level of firing of the input for the sensor
	  (assumes only one input node currently)  */
	switch (theSensor.system) {
		case GUT_SENSOR:
			for (counter=0; counter<TYPES;counter++) 
				if (theGut[counter]>0)
					trigger += theSensor.complex[counter];
			break;
		case CONTACT: 
			for (counter=0;counter<TYPES; counter++) detectedYet[counter] = FALSE;
			sensedCells = check_world (org_pos, org_direction, theSensor);
			if (sensedCells!=NIL_POINTER) {

				/* Add levels of triggering of the sensor.
			   	   Only detect presense 
				   of atoms, not number.  */

				cellPointer = world[sensedCells->current.x][sensedCells->current.y];
				while (cellPointer!=NIL_POINTER) {
					if (cellPointer->type == food)
						if (!detectedYet[cellPointer->datum.atom]) {
							trigger+=theSensor.complex[cellPointer->datum.atom];
							detectedYet[cellPointer->datum.atom]=TRUE;
						}
					cellPointer = cellPointer->next;
				}
			}
			break;
		case AMBIENT:
			sensedCells = check_world (org_pos, org_direction, theSensor);
			while (sensedCells!=NIL_POINTER) {
				for (counter=0;counter<TYPES; counter++) detectedYet[counter] = FALSE;
				cellPointer = world[sensedCells->current.x][sensedCells->current.y];
				while (cellPointer!=NIL_POINTER) {
					if (cellPointer->type == food)
						if (!detectedYet[cellPointer->datum.atom]) {
							trigger+=(theSensor.complex[cellPointer->datum.atom]/sensedCells->distance);
							detectedYet[cellPointer->datum.atom]=TRUE;
						}
					cellPointer = cellPointer->next;
				}
				sensedCells = sensedCells->next;
			}
			break;
		default:
			printf("Error:  Unknown sensor type.\n");
			exit(1);
	}


	/* Normalize the trigger level to 1 and max out at 1. 
	   Place the final firing value for the input node in
	   an array that represents the input nodes of the 
	   neural network.  */

	switch (theSensor.system) {
		case AMBIENT:
			trigger/=norm_dist[theSensor.range];
		case GUT_SENSOR:
		case CONTACT:
			trigger/=COMPLEX_SIZE;

			for (counter=0; counter<(*layer_descp);counter++) 
				if (theSensor.mask[counter]!=0) {
					activity_levels[counter] = trigger;
					if (maskCheck) {
						printf("\nSensor masking to more than one input!\nCode is not designed for this.\n");
						exit(1);
					}
					else 
						maskCheck = TRUE;
				}
			break;

		default:
			printf("Error:  Unknown sensor type.\n");
			exit(1);
	}

	killListOfPos(sensedCells);
}
	


/*  Sense the world using all the sensors (which place
    their firing levels for their input nodes in a temporary
    array), combine all their inputs, and place
    the combined input to the network in the network.  */

void	
sense_world (organism)
	struct indiv	*organism;
{
	int		x,y;
	float		*new_levels, *activity_levels;
	position	org_pos;
	int		numInputs = (*layer_descp);


	org_pos.x=organism->worldx;
	org_pos.y=organism->worldy;

	activity_levels = (float *) malloc(NINPUTS * sizeof(float));
	new_levels = (float *) malloc(NINPUTS * sizeof(float));
	if ((activity_levels == NIL_POINTER) || (new_levels == NIL_POINTER))
	{
		perror("ERROR: levels malloc");
		exit(1);
	}

	for (x=0; x<numInputs; x++) 
		*(activity_levels + x) = 0;

	
	/* Total the values that the individual sensors will
		place on the inputs of the neural net */

	for (x=0; x<NSENSORS; x++) {
		for (y=0; y<numInputs; y++) 
			*(new_levels + y) = 0;

		sense(org_pos, organism->direction, organism->sensor_specs[x], organism->gut, new_levels);

		for (y=0;y<numInputs; y++) 
			activity_levels[y]+=new_levels[y];
	}


	/* Put activity_levels vector as values on inputs of neural network */

	for (x=0; x<numInputs; x++)
		*(*(organism->layerp)+x) = activity_levels[x];


	free((char *) activity_levels);
	free((char *) new_levels);
	
}






/*	Given the direction of the organism and the
	location of the sensor/motor, calculates the 
	absolute facing of that unit.  This can then
	be used to determine the direction of 
	movement/sensing. */


int
facing (org_direction, object_loc)
	int 		org_direction, object_loc;
{
	int		heading;
	switch (object_loc) {
		case BACKWARD:
			switch (org_direction) {
				case UP:
					heading = DOWN;
					break;
				case DOWN:
					heading = UP;
					break;
				case LEFT:
					heading = RIGHT;
					break;
				case RIGHT:
					heading = LEFT;
					break;
				default:
					printf("Error in facing().  Undefined direction.\n");
					exit(1);
			}
			break;
		case LEFT_SIDE:
			switch (org_direction) {
				case UP:
					heading = LEFT;
					break;
				case DOWN:
					heading = RIGHT;
					break;
				case LEFT:
					heading = DOWN;
					break;
				case RIGHT:
					heading = UP;
					break;
				default:
					printf("Error in facing().  Undefined direction.\n");
					exit(1);
			}
			break;
		case RIGHT_SIDE:
			switch (org_direction) {
				case UP:
					heading = RIGHT;
					break;
				case DOWN:
					heading = LEFT;
					break;
				case LEFT:
					heading = UP;
					break;
				case RIGHT:
					heading = DOWN;
					break;
				default:
					printf("Error in facing().  Undefined direction.\n");
					exit(1);
			}
			break;
		case FORWARD:
			heading = org_direction;
			break;
		default:
			printf("Error in facing().  Undefined location.\n");
			exit(1);
	}

	return (heading);
}









/*	Returns the change in position that a binary motor 
	produces */

position 
binaryMotorMove (outputLayer, numOutputs, theMotor, newFacing) 
	float*			outputLayer;
	int			numOutputs;
	motor			theMotor;
	int			*newFacing;	/* Pass by location */
{
	position	diff;
	int		org_direction = *newFacing;
	int		command=0, index, bin_val=2;


	diff.x = 0;
	diff.y = 0;

	/* A binary motor is based off two bit values.  
	   The masked values must be converted.  Anything
           above .5 will be considered 1 */

	for (index=0; index<numOutputs;index++) 
		if (theMotor.mask[index]==1) {
			if (*(outputLayer+index)>.5) {
				command+=bin_val;
			}
			bin_val/=2;
			if (bin_val<1)
				bin_val = 0;
		}
	

	switch (command) {
		case 0: 
			break;
		case 1:
                        *newFacing = facing (org_direction, RIGHT_SIDE);
			break;
		case 2:
			*newFacing = facing (org_direction, LEFT_SIDE);
			break;
		case 3:
			/* ADD LOCATION OF MOTOR HERE! */
			diff = moveDiff(org_direction);
			diff.x *= theMotor.power;
			diff.y *= theMotor.power;
			break;
	}


	return (diff);
}




/*	Move the organism as indicated by the neural 
	network */

void move (organism) 
	struct indiv	*organism;
{
	int			counter;
	position	org_pos, posDiff, tempDiff, new_pos;
	int			newFacing = organism->direction;
	boolean		isSelected=FALSE, isFound = FALSE;
	cell		*theCell;
	int			orgItemNum = 0;



	org_pos.x=organism->worldx;
	org_pos.y=organism->worldy;

	posDiff.x = 0;
	posDiff.y = 0;
	
	theCell = world[org_pos.x][org_pos.y];
	
	while ((theCell->next!=NULL) && (!isFound)) {
		if ((theCell->type==org) && (theCell->datum.ap==organism))
			isFound = TRUE;
		else {
			theCell = theCell->next;
			orgItemNum++;
		}
	}
	
	if ((theCell->type!=org) || (theCell->datum.ap!=organism)) {
		printf("Couldn't find the organism in the cell!\n");
		exit(1);
	}


	/* Motor movements are additive to form
	   total movement */

	for (counter=0; counter<NMOTORS; counter++) {
		switch (organism->motor_specs[counter].system) {
			case BINARY:
				tempDiff = binaryMotorMove(*(organism->layerp+nlayers-1), (*(layer_descp+nlayers-1)), organism->motor_specs[counter], &newFacing);
				posDiff.x += tempDiff.x;
				posDiff.y += tempDiff.y;

				organism->energy-=BINARY_ENERGY_USE;
				break;
			default:
				printf("Unknown motor type!");
				exit(1);
		}
	}

	new_pos = find_cell(org_pos, posDiff);

	if ((new_pos.x!=organism->worldx) || (new_pos.y!=organism->worldy)) {

		moveInteractiveLasso(orgItemNum, &isSelected, organism);
		
		del_org(organism);
			
		organism->worldx = new_pos.x;
		organism->worldy = new_pos.y;
		organism->direction = newFacing;
		
		ins_org(organism);
		
		moveInteractDrawSelectFrame(isSelected, new_pos);

	}
	else 
		organism->direction = newFacing;
}



/*	Given two cells, computes the horizontal
	and vertical distance between them, 
	compensating for the toroidal world. */

position	distance (initCell,finalCell)
	position	initCell, finalCell;
{
	position	diff;
	int		deltaX, deltaY;

	deltaX = finalCell.x - initCell.x;
	deltaY = finalCell.y - initCell.y;

	if (initCell.x<finalCell.x) {
		if (deltaX>abs(deltaX - XMAX)) {
			deltaX -= XMAX;
		}
	}
	else {
		if (abs(deltaX)>(deltaX+XMAX)) {
			deltaX += XMAX;
		}
	}


	if (initCell.y<finalCell.y) {
		if (deltaY>abs(deltaY - YMAX)) {
			deltaY -= YMAX;
		}
	}
	else {
		if (abs(deltaY)>(deltaY+YMAX)) {
			deltaY += YMAX;
		}
	}

	diff.x = deltaX;
	diff.y = deltaY;

	return(diff);
}



/*
 * called in populati.c;
 * returns the number of input units allocated to the sensor
 * ATTENTION: for now there is only the contact sensor system
 */
sensor_size(system)

        int system;
{
        switch(system)
        {
                case GUT_SENSOR:
                case AMBIENT:
                case CONTACT:
                        return(1);
                        break;
                default:
                        printf("error: sensor system unknown\n");
                        exit(1);
        }
}





/*
 * called in populati.c;
 * returns the number of output units allocated to the motor
 * ATTENTION: for now there is only the binary motor system
 */
motor_size(system)

        int system;
{
        switch(system)
        {
                case BINARY:   
                                return(2);
                                break;
                default:        printf("error: motor system unknown\n");
                                exit(1);
        }
}



/*
 * called in populati.c;
 * returns a sensor/motor mask for new individuals
 */
int get_mask(count, size, i)

        int count;      /* end unit for this mask */
        int size;       /* number of units for this mask */
        int i;          /* unit position */
{
        if ((i < count) && (i >= (count - size))) return(1);
        else return(0);
}

/*
 * called in populati.c;
 * returns a motor power for new individuals
 *
 * ATTENTION: for now there is only one binary motor
 */
int motor_power()

{
        return(BIN_POWER);
}



/*
 * called by mutate() in populati.c;
 * mutates sensors' complexes and motors' powers
 * for new individuals:
 * sensitivity in the sensor complex is shifted
 * from one atom to another, and the motor power
 * is added a random integer in [-1,+1] while
 * kept >= 0;
 * ATTENTION: for now, orientation and position
 *            of sensors and motors are not mutated
 */
mu_sensorymotor(ap)

        struct indiv *ap;
{
        int             i,j;

        if ((0.5 + rans(0.5)) < MU_SENSOR_PROB)
         for (i=0; i<NSENSORS; i++)
         {
                do {
                        j = mrand(TYPES);
                   } while (ap->sensor_specs[i].complex[j] == 0);
                --ap->sensor_specs[i].complex[j];
                ++ap->sensor_specs[i].complex[mrand(TYPES)];
         }
        if ((0.5 + rans(0.5)) < MU_MOTOR_PROB)
         for (i=0; i<NMOTORS; i++)
         {
                ap->motor_specs[i].power += (mrand(3) -1);
                while (ap->motor_specs[i].power < 0)
                        ++ap->motor_specs[i].power;
         }
}


