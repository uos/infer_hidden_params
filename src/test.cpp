
#include <string>
#include <iostream>

#include <ros/ros.h>
#include <json_prolog/prolog.h>

using namespace std;
using namespace json_prolog;

int main(int argc, char *argv[])
{
  ros::init(argc, argv, "test");

  Prolog pl;

  // Test Query 

  string a = "project_action_effects(drink_world:'FillingProcess1')";

  PrologQueryProxy test = pl.query(a);
/*  cout << "Drinks of Red Color: ";
  for(PrologQueryProxy::iterator it=test.begin(); it != test.end(); it++)
  {
    PrologBindings te = *it;
    cout << te["Obj"] << ", ";
  }
  cout << endl<< endl;
*/
  return 0;
}
