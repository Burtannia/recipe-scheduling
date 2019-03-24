min: 1.0Emax;
1.0Emax >= 1.0E_0;
1.0Emax >= 1.0E_1;
1.0E_0 >= 1.0E_0_chef;
1.0E_1 >= 1.0E_1_kettle;
1.0E_1_kettle >= 1.0E_0_chef + 120.0X_1_kettle;
1.0E_0_chef >= 10.0X_0_chef;
1.0E_1_kettle >= 120.0X_1_kettle;

// this doesn't work
//1.0X_0_chef = 1.0;
//1.0X_1_kettle = 1.0;

// but this does...
1.0X_0_chef + x1 = 1.0;
1.0X_1_kettle + x2 = 1.0;
x1 + x2 = 0;

bin X_0_chef,X_1_kettle;
bin Y_0_chef_0,Y_1_kettle_1;