/********** INIT *********************************************************************************/
#define INIT_NODE_NUM 64

#include <iostream>
#include <vector>
#include <memory>

using std::cout;
using std::cerr;
using std::endl;
using std::vector;
using std::shared_ptr;


/********** NODE CLASSES *************************************************************************/

/***** Node *****/

class Node{ public:


std::shared_ptr<void> data;

template<typename T>
auto get(){
    return *(std::static_pointer_cast<T>(data));
}

// void set(const std::shared_ptr<void>& p) {
//    data = p;
// }

template<typename T>
void set(const T& p){
   data = std::make_shared<void>( p );
}

};




/********** TEST/MAIN ****************************************************************************/
int main(){

cout << "Test 1" << endl;
Node /*----*/ foo{};




cout << endl << endl;

cout << "Complete!" << endl;
return 0;
}