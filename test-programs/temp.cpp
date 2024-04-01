int main() {
  {
    int a = 1;
  }
}

// int main(){
//     int a = 1, b;
//     {
//         a = 2;
//         int a = 3;
//         a = 4;
//     }
//     {
//         int a = 5;
//         {
//             int a = 6;
//             a = 7;
//         }
//         a = 8;
//     }
//     a = 9;
//     return 0;   // Should return -1
// }