/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
* 
* Created by: Lars Hagström / stlrha
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

using System;
//using System.Collections.Generic;
//using System.Text;
using System.Runtime.InteropServices;

namespace Safir.Dob
{
    /// <summary>
    /// Class used for responding to received requests.
    /// <para/>
    /// The purpose of this class is to allow responses to be sent either from 
    /// within the request callback, or at a later time (in which case you have
    /// to keep the responseSender you received in the callback "for later").
    /// <para/>
    /// Note that you still have to send the response within the timout period,
    /// or the response will not be delivered to the requestor (who will have
    /// received a timeout response instead).
    /// </summary>
    public sealed class ResponseSender
    {
        internal ResponseSender(System.Int32 ctrl,
                                System.IntPtr consumer,
                                System.Int32 responseId)
        {
            m_valid=true;
            m_ctrl = ctrl;
            m_consumer = consumer;
            m_responseId = responseId;
        }

        /// <summary>
        /// Sends a response for the request that this instance was obtained with.
        /// <para/>
        /// This method may only be called once on any given instance! Calling it twice
        /// amounts to trying to send two responses to one request, which is considered
        /// a programming error.
        /// </summary>
        /// <param name="response">The response to be sent.</param>
        public void Send(Dob.Response response)
        {
            if (!m_valid)
            {
                throw new Dob.AccessDeniedException("ResponseSender object has already been used once.");
            }
            byte success;

            //TODO: serialize directly to shared memory
            System.Int32 blobSize = response.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize); //allocate blob
            System.IntPtr beginningOfUnused;
            Typesystem.Internal.InternalOperations.FormatBlob(blob, blobSize, response.GetTypeId(), out beginningOfUnused);
            response.WriteToBlob(blob, ref beginningOfUnused);

            Interface.DoseC_SendResponse(m_ctrl, blob, m_consumer, 
                Interface.DOSE_LANGUAGE_DOTNET, m_responseId, out success);

            Marshal.FreeHGlobal(blob); //delete blob

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            m_valid = false;
        }

        /// <summary>
        /// Check if an instance is still waiting for a response to be sent.
        /// </summary>
        /// <returns>True if a response has been sent using this instance, otherwise false.</returns>
        public bool IsDone()
        {
            return !m_valid;
        }

        /// <summary>
        /// Discard this ResponseSender.
        /// <para/>
        /// Calling this function means that you forfeit your chance to send a response
        /// to the request. It will disable the checks in the destructor (see above).
        /// <para/>
        /// The typical case when you must discard the ResponseSender is when calling
        /// Postpone with redispatchCurrent set to True. In this case you will get
        /// the request again together with a new ResponseSender.
        /// </summary>
        public void Discard()
        {
            if (!m_valid)
            {
                throw new Safir.Dob.Typesystem.SoftwareViolationException
                    ("ResponseSender object has already been used once.");
            }
            m_valid = false;
        }

        /// <summary>
        /// Destructor.
        /// <para/>
        /// Will check that the ResponseSender has been used, and if it hasn't a dialog
        /// will be shown and the program terminated.<para/>
        /// Since the destructor is called by the Garbage Collector this may happen 
        /// "long after" you've dropped your reference to the response sender.
        /// <para/>
        /// Not using a ResponseSender is considered a programming error.
        /// </summary>
        ~ResponseSender()
        {
            if (m_valid)
            {
                System.Windows.Forms.MessageBox.Show("Programming Error! A ResponseSender was discarded without having been used! (A timeout response was automatically sent).\nThe program will now exit!");
                System.Environment.Exit(101010);
            }
        }

        #region Private stuff
        private bool m_valid;
        private System.Int32 m_ctrl;
        private System.IntPtr m_consumer;
        private System.Int32 m_responseId;
        #endregion
    }
}
