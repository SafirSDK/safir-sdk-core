/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include <iostream>

//This is not really a test, it is just useful for showing what environment
//variables are set for tests by cmake and ctest etc.
//Change the return code to 1 if you want to use ctest --output-on-failure.
int main(int argc, char **argv, char** envp)
{
  char** env;
  for (env = envp; *env != 0; env++)
  {
    char* thisEnv = *env;
    std::cout << thisEnv << std::endl;
  }

  return 0;
}
