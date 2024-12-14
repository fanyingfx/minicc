int main(void){
    int x = 0;
    if (x<=0) {
        int a = 1; 
        
        int b = a+1;
        {
            int c = b+1;
            x = c+1;
        }
    }
    return x;
}