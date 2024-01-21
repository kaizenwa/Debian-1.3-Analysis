typedef struct {
	int a;
} AStruct;

void MakeBug() {
	AStruct *job;

	// This used to crash, it should now give error(s).
	job = new AStruct[];

	job = new AStruct;
}

main () {
	MakeBug();
}
