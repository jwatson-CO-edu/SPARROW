enum mynewenum
{
   RATS,
   SILLY, 
   DUNWORK
};
 
class class1
{
   public:
      mynewenum mne;
 
      class1(mynewenum c)
      {
         mne = c;          
      }
};
 
int main()
{
   class1
         a1(RATS);
 
   return 0;
}