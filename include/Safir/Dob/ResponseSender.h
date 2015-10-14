/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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
#ifndef _SAFIR_DOB_RESPONSESENDER_H
#define _SAFIR_DOB_RESPONSESENDER_H

#include <Safir/Dob/DoseCppExportDefs.h>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>
#include <Safir/Dob/Response.h>

namespace Safir
{
namespace Dob
{
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4275)
#endif


    /**
     * Class used for responding to received requests.
     *
     * The purpose of this class is to allow responses to be sent either from 
     * within the request callback, or at a later time (in which case you have
     * to keep the responseSender you received in the callback "for later").
     *
     * Note that you still have to send the response within the timout period,
     * or the response will not be delivered to the requestor (who will have
     * received a timeout response instead).
     */
    class DOSE_CPP_API ResponseSender:
        private boost::noncopyable
    {
    public:
        /**
         * Destructor.
         *
         * Will check that the ResponseSender has been used, and if it
         * hasn't an error will be reported (a PanicLog!).
         * 
         * Not using a ResponseSender is considered a programming error.
         */
       virtual ~ResponseSender() {}

        /**
         * Sends a response for the request that this instance was obtained with.
         *
         * This method may only be called once on any given instance! Calling it twice
         * amounts to trying to send two responses to one request, which is considered
         * a programming error.
         *
         * @param response - The response to be sent.
         * @exception Safir::Dob::NotOpenException  - If the connection is not open.
         */
        virtual void Send(const Safir::Dob::ResponsePtr & response) = 0;

        /**
         * Check if an instance is still waiting for a response to be sent.
         *
         * @return True if a response has been sent using this instance
         *         (instance is consumed), otherwise false.
         */
        virtual bool IsDone() = 0;

        /**
         * Discard this ResponseSender.
         *
         * Calling this function means that you forfeit your chance to send a response
         * to the request. It will disable the checks in the destructor (see above).
         *
         * The typical case when you must discard the ResponseSender is when calling
         * Postpone with redispatchCurrent set to True. In this case you will get
         * the request again together with a new ResponseSender.
         */
        virtual void Discard() = 0;

    };
#ifdef _MSC_VER
#pragma warning(pop)
#endif

    //-------------------------------
    // Smart pointer to ResponseSender
    //-------------------------------
    typedef boost::shared_ptr<ResponseSender> ResponseSenderPtr;
}
}

#endif
