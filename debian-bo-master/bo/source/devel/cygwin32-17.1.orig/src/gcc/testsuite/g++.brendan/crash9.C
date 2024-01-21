class A {};

class SimQuery
{
public:
  SimQuery();
  ~SimQuery();
  int SetMeshFile(char name[]);
protected:
  A& scaling;
  A* mesh;
};

SimQuery::SimQuery():scaling(A) {}

SimQuery::~SimQuery() {}

int SimQuery::SetMeshFile(char name[])
{
  mesh = new C;
}
