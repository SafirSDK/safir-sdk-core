/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
* 
* Created by: Joel Ottosson / stjoot
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

#include "dots_base64_conversions.h"
#include <math.h>
#include <assert.h>
#include <stdlib.h>
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    const int  LINE_LENGTH  =   64;
    const char  LINE_BREAK   =   '\n';
    const char  ENCODING[64] =   {
                                'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
                                'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
                                'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                                'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/'
                                };

    Int32 Base64Conversions::CalculateBase64Size(const Int32 sourceSize)
    {
        const div_t res = div(static_cast<int>(sourceSize),3);
        const int numChars = res.quot*4 + (res.rem?4:0);
        const div_t numNewLines = div(numChars,LINE_LENGTH);
        return numChars + numNewLines.quot -(numNewLines.rem==0?1:0);
    }

    Int32 Base64Conversions::CalculateBinarySize(const Int32 sourceSize)
    {
        int requiredSize=(3*sourceSize)/4; //not exactly correct, theres a small overhead due to linebreaks.
        return requiredSize;
    }

    bool Base64Conversions::ToBase64(char* base64Dest,
                                     Int32 destCapacity,
                                     const char * const binarySource,
                                     Int32 sourceSize,
                                     Int32 & resultSize )
    {
        //check buffer capacity     3
        int requiredSize=CalculateBase64Size(sourceSize);
        if (destCapacity<requiredSize)
        {
            resultSize=requiredSize;
            return false;
        }

        //Buffer size big enough, do conversion
        resultSize=0;
        int pos=0;
        int thisRow=0;
        while(pos<sourceSize)
        {
            if (thisRow==64)
            {
                base64Dest[resultSize++]=LINE_BREAK;
                thisRow=0;
            }
            if (pos+3<=sourceSize)
            {
                Bit24ToBase64(base64Dest+resultSize, binarySource+pos);
            }
            else if (pos+2==sourceSize)
            {
                char bin[3]={binarySource[pos], binarySource[pos+1], 0};
                Bit24ToBase64(base64Dest+resultSize, bin);
                resultSize+=4;
                base64Dest[resultSize-1]='=';
                assert(resultSize == destCapacity);
                return true;
            }
            else
            {
                char bin[3]={binarySource[pos], 0, 0};
                Bit24ToBase64(base64Dest+resultSize, bin);
                resultSize+=4;
                base64Dest[resultSize-1]='=';
                base64Dest[resultSize-2]='=';
                assert(resultSize == destCapacity);
                return true;
            }

            pos+=3;
            resultSize+=4;
            thisRow+=4;
        }
        assert(resultSize == destCapacity);
        return true;
    }

    void Base64Conversions::Bit24ToBase64(char* b64, const char* const bin)
    {
        //converts 3 bytes into 4 Base64 chars
        b64[0] = ENCODING[(bin[0]>>2) & 0x3F];
        b64[1] = ENCODING[((bin[0] << 4) & 0x30) | ((bin[1] >> 4) & 0xF)];
        b64[2] = ENCODING[((bin[1] << 2) & 0x3C) | ((bin[2] >> 6) & 0x3)];
        b64[3] = ENCODING[bin[2] & 0x3F];
    }

    bool Base64Conversions::ToBinary(char* binaryDest,
                                     Int32 destCapacity,
                                     const char * const base64Source,
                                     Int32 sourceSize,
                                     Int32 & resultSize)
    {
        //check buffer capacity
        int requiredSize=CalculateBinarySize(sourceSize);
        if (destCapacity<requiredSize)
        {
            resultSize=requiredSize;
            return false;
        }

        //buffer size enough, continue with conversion
        resultSize=0;
        int pos=0;
        int letterCounter=0;
        char b64[4];
        while(pos<sourceSize)
        {
            if (base64Source[pos]!=LINE_BREAK)
            {
                b64[letterCounter++]=base64Source[pos];
            }

            if (letterCounter==4)
            {
                Base64ToBit24(binaryDest+resultSize, b64);
                resultSize+=3;
                letterCounter=0;
            }

            ++pos;
        }

        if (b64[2]=='=')
            resultSize-=2;
        else if (b64[3]=='=')
            --resultSize;

        return true;
    }

    void Base64Conversions::Base64ToBit24(char* bin, const char* const b64)
    {
        //Converts 4 Base64 chars into 3 bytes of binary data
        char code[4];
        for (int i=0; i<4; i++)
        {
            if (b64[i]>='0' && b64[i]<='9')
                code[i]=b64[i]+4;
            else if (b64[i]>='A' && b64[i]<='Z')
                code[i]=b64[i]-65;
            else if (b64[i]>='a' && b64[i]<='z')
                code[i]=b64[i]-71;
            else if (b64[i]=='+')
                code[i]=62;
            else if (b64[i]=='/')
                code[i]=63;
        }

        bin[0] = ((code[0] << 2) & 0xFC) | ((code[1] >> 4) & 0x3);
        bin[1] = ((code[1] << 4) & 0xF0) | ((code[2] >> 2) & 0xF);
        bin[2] = ((code[2] << 6) & 0xC0) | (code[3] & 0x3F);
    }
}
}
}
}
