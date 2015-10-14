/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace Sate
{
    public class TreeViewImageHandler
    {  
        //***********************************************************
        // Image Index consts
        //***********************************************************
        private const int NS                            = 0;                
        private const int EntityDefault                 = 1;                       
        private const int ServiceDefault                = 9;                
        private const int ObjectDefault                 = 17;               
        private const int MessageDefault                = 25;               
        private const int Response                      = 27;
        private const int Item                          = 28;
        private const int Struct                        = 29;
        private const int Parameters                    = 30;
        private const int ClassDefault                  = 31;
        private const int FirstCustomBitmap             = ClassDefault +1;

        //Offsets
        private const int SubscribedOffset              = 1;
        private const int RegisteredOffset              = 2;
        private const int PendingOffset                 = 3;
        private const int OwnedOffset                   = 4;
        private const int RegisteredSubscribedOffset    = 5;
        private const int PendingSubscribedOffset       = 6;
        private const int OwnedSubscribedOffset         = 7;
        private const int NextBitmapOffset              = OwnedSubscribedOffset + 1;                
        //***********************************************************        

        public enum ImageType
        {
            Namespace = 0,

            Default,
            Registered,
            Pending,
            Owned,
            Subscribed,
            RegisteredSubscribed,
            PendingSubscribed,
            OwnedSubscribed   
        }       
        
        private System.Windows.Forms.ImageList imageList = new System.Windows.Forms.ImageList();
        private System.Windows.Forms.ImageList defaultImagesList = new System.Windows.Forms.ImageList();

        private System.Collections.Hashtable PathIndexHt = new System.Collections.Hashtable();        

        public TreeViewImageHandler(System.Windows.Forms.ImageList imageList, System.Windows.Forms.ImageList defaults)
        {
            this.imageList = imageList;
            this.defaultImagesList = defaults;                                                      
        }

        public void RefreshImageCollection()
        {
            imageList.Images.Clear();
            PathIndexHt.Clear();

            //
            //Create needed images from default image list   
            //
            Bitmap subBitmap = null;
            imageList.Images.Add(defaultImagesList.Images[0]);                           //pacakge(namespace)

            imageList.Images.Add(defaultImagesList.Images[1]);                           //entity default
            subBitmap = Create_S_Bitmap((Bitmap)defaultImagesList.Images[1]);
            imageList.Images.Add(subBitmap);                                    //entity subscribe
            imageList.Images.Add(Create_R_Bitmap((Bitmap)defaultImagesList.Images[1]));  //entity regsiter
            imageList.Images.Add(Create_P_Bitmap((Bitmap)defaultImagesList.Images[1]));  //entity pending
            imageList.Images.Add(Create_O_Bitmap((Bitmap)defaultImagesList.Images[1]));  //entity own
            imageList.Images.Add(Create_R_Bitmap(subBitmap));                   //entity reg+sub
            imageList.Images.Add(Create_P_Bitmap(subBitmap));                   //entity pend+sub
            imageList.Images.Add(Create_O_Bitmap(subBitmap));                   //entity own+sub

            imageList.Images.Add(defaultImagesList.Images[2]);                           //service default
            subBitmap = Create_S_Bitmap((Bitmap)defaultImagesList.Images[2]);
            imageList.Images.Add(subBitmap);                                    //service subscribe
            imageList.Images.Add(Create_R_Bitmap((Bitmap)defaultImagesList.Images[2]));  //service regsiter
            imageList.Images.Add(Create_P_Bitmap((Bitmap)defaultImagesList.Images[2]));  //service pending
            imageList.Images.Add(Create_O_Bitmap((Bitmap)defaultImagesList.Images[2]));  //service own
            imageList.Images.Add(Create_R_Bitmap(subBitmap));                   //service reg+sub
            imageList.Images.Add(Create_P_Bitmap(subBitmap));                   //service pend+sub
            imageList.Images.Add(Create_O_Bitmap(subBitmap));                   //service own+sub

            imageList.Images.Add(defaultImagesList.Images[3]);                           //object default
            subBitmap = Create_S_Bitmap((Bitmap)defaultImagesList.Images[3]);
            imageList.Images.Add(subBitmap);                                    //object subscribe
            imageList.Images.Add(Create_R_Bitmap((Bitmap)defaultImagesList.Images[3]));  //object regsiter
            imageList.Images.Add(Create_P_Bitmap((Bitmap)defaultImagesList.Images[3]));  //object pending
            imageList.Images.Add(Create_O_Bitmap((Bitmap)defaultImagesList.Images[3]));  //object own
            imageList.Images.Add(Create_R_Bitmap(subBitmap));                   //object reg+sub
            imageList.Images.Add(Create_P_Bitmap(subBitmap));                   //object pend+sub
            imageList.Images.Add(Create_O_Bitmap(subBitmap));                   //object own+sub

            imageList.Images.Add(defaultImagesList.Images[4]); //message default
            imageList.Images.Add(Create_S_Bitmap((Bitmap)defaultImagesList.Images[4])); //message subscribe

            imageList.Images.Add(defaultImagesList.Images[5]); //response
            imageList.Images.Add(defaultImagesList.Images[6]); //item
            imageList.Images.Add(defaultImagesList.Images[7]); //struct
            imageList.Images.Add(defaultImagesList.Images[8]); //parameters
            imageList.Images.Add(defaultImagesList.Images[9]); //clss default


            //
            //Create custom images
            //                     
            foreach (ImageMapping mapping in Settings.Sate.ImageMappings)
            {
                if (PathIndexHt[mapping.ImagePath] == null)
                {
                    int imageListIndex=LoadCustomImage(mapping.ImagePath);
                    if (imageListIndex > -1)
                    {
                        PathIndexHt[mapping.ImagePath] = imageListIndex;
                    }
                }
            }         
        }

        public int NamespaceImageIndex
        {
            get {return NS;}
        }

        public int GetImageIndex(long typeId, ImageType imageType)
        {
            int defaultIndex = -1;                    

            try
            {
                if (Settings.Sate.ImageMappings.Length > 0)
                {
                    ImageMapping found = null;                    
                                        
                    foreach (ImageMapping mapping in Settings.Sate.ImageMappings)
                    {
                        long mapTypeId;
                        bool isClass = System.Int64.TryParse(mapping.MapObject, out mapTypeId);

                        if (isClass)
                        {
                            if (mapTypeId == typeId)
                            {
                                found = mapping;                                
                                break;
                            }
                            else if (mapping.Inherit && Safir.Dob.Typesystem.Operations.IsOfType(typeId, mapTypeId))
                            {
                                if (found == null)
                                {
                                    found = mapping;
                                }
                                else
                                {
                                    System.Int64 foundTypeId;
                                    if (System.Int64.TryParse(found.MapObject, out foundTypeId))
                                    {
                                        if (Safir.Dob.Typesystem.Operations.IsOfType(foundTypeId, mapTypeId))
                                        {
                                            //closer ancestor to our type
                                            found = mapping;
                                        }
                                    }           
                                    //note: namespace custom images always overrides class custom images
                                }                               
                            }
                        }
                        else //namespace
                        {
                            string typeName=Safir.Dob.Typesystem.Operations.GetName(typeId);
                            string typeNamespace=typeName.Substring(0, typeName.LastIndexOf('.'));
                            if (typeNamespace == mapping.MapObject)
                            {
                                found = mapping;
                                break;
                            }
                            else if (mapping.Inherit && typeNamespace.StartsWith(mapping.MapObject))
                            {
                                if (found == null)
                                {
                                    found = mapping;
                                }
                                else
                                {
                                    System.Int64 foundTypeId;
                                    if (!System.Int64.TryParse(found.MapObject, out foundTypeId))
                                    {
                                        if (mapping.MapObject.StartsWith(found.MapObject))
                                        {
                                            //closer namespace to our type
                                            found = mapping;
                                        }
                                    }
                                    else
                                    {
                                        //note: namespace custom images always overrides class custom images
                                        found = mapping;
                                    }
                                }                                
                            }
                        }
                        
                        
                    }

                    if (found!=null && PathIndexHt[found.ImagePath] != null)
                    {
                        defaultIndex = (int)PathIndexHt[found.ImagePath];
                    }    
                }
            }
            catch { }                                

            //If no custom bitmap, use standard bitmaps
            if (defaultIndex < 0)
            {
                defaultIndex = GetStandardDefaultIndex(typeId);
            }

            return ApplyImageType(defaultIndex, imageType);
        }

        public int GetImageIndex(Safir.Dob.Typesystem.EntityId entityId, ImageType imageType)
        {
            return ApplyImageType(ObjectDefault, imageType);
            /*
            if (oid.Instance >= 0) //object icon
            {
                return ApplyImageType(ObjectDefault, imageType);                
            }
            else //class
            {
                return GetImageIndex(oid.TypeId, imageType);         
            } 
            */
        }

        //creates all custom images from a file, returns index in imagelist, -1=fileNotFound
        private int LoadCustomImage(string path)
        {                                  
            try
            {
                int index = imageList.Images.Count;                
                System.Drawing.Bitmap bmDefault = new System.Drawing.Bitmap(path);

                if (bmDefault.Width != 16 || bmDefault.Height != 16)
                {
                    bmDefault = new Bitmap(bmDefault, new System.Drawing.Size(16, 16));
                }

                System.Drawing.Bitmap bmSubscribed = Create_S_Bitmap(bmDefault);
                System.Drawing.Bitmap bmRegistered = Create_R_Bitmap(bmDefault);
                System.Drawing.Bitmap bmPending = Create_P_Bitmap(bmDefault);
                System.Drawing.Bitmap bmOwned = Create_O_Bitmap(bmDefault);
                System.Drawing.Bitmap bmRegSub = Create_R_Bitmap(bmSubscribed);
                System.Drawing.Bitmap bmPendSub = Create_P_Bitmap(bmSubscribed);
                System.Drawing.Bitmap bmOwnedSub = Create_O_Bitmap(bmSubscribed);                              

                imageList.Images.AddRange(new Image[]{ bmDefault, 
                                                    bmSubscribed,
                                                    bmRegistered,
                                                    bmPending,
                                                    bmOwned,
                                                    bmRegSub,
                                                    bmPendSub,
                                                    bmOwnedSub});                   
                return index;
            }
            catch
            {              
                return -1;
            }            
        }

        

        private int GetStandardDefaultIndex(long typeId)
        {
            if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Entity.ClassTypeId))
            {
                return EntityDefault;
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Message.ClassTypeId))
            {
                return MessageDefault;
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Service.ClassTypeId))
            {
                return ServiceDefault;
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Response.ClassTypeId))
            {
                return Response;
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Item.ClassTypeId))
            {
                return Item;
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Struct.ClassTypeId))
            {
                return Struct;
            }
            else if (Safir.Dob.Typesystem.Operations.IsOfType(typeId, Safir.Dob.Parametrization.ClassTypeId))
            {
                return Parameters;
            }
            else
            {
                return ClassDefault;
            }
        }

        private int ApplyImageType(int defaultIndex, ImageType imageType)
        {
            int index = defaultIndex;
            switch (imageType)
            {
                case ImageType.Registered:
                    index += RegisteredOffset;
                    break;
                case ImageType.Pending:
                    index += PendingOffset;
                    break;
                case ImageType.Owned:
                    index += OwnedOffset;
                    break;
                case ImageType.Subscribed:
                    index += SubscribedOffset;
                    break;
                case ImageType.RegisteredSubscribed:
                    index += RegisteredSubscribedOffset;
                    break;
                case ImageType.PendingSubscribed:
                    index += PendingSubscribedOffset;
                    break;
                case ImageType.OwnedSubscribed:
                    index += OwnedSubscribedOffset;
                    break;
            }

            if (index < 0 || index >= imageList.Images.Count)
                return ClassDefault;
            else
                return index;            
        }        

        //----------------------------------------------------------------
        // Create image methods, adds S, R, O, and P to an image. 
        // To create a Reg+Sub image combine these methods like this:
        // Bitmap regSub=Create_R_Bitmap(Create_S_Bitmap(orginalImage));
        //----------------------------------------------------------------
        private Bitmap Create_S_Bitmap(Bitmap b) //add S to input image
        {
            //  +-----+
            //  |    S|
            //  |     |
            //  +-----+
            Bitmap bitmap = new Bitmap(b);
            bitmap.SetPixel(12, 2, Color.Black);
            bitmap.SetPixel(12, 5, Color.Black);
            bitmap.SetPixel(13, 1, Color.Black);
            bitmap.SetPixel(13, 3, Color.Black);
            bitmap.SetPixel(13, 5, Color.Black);
            bitmap.SetPixel(14, 1, Color.Black);
            bitmap.SetPixel(14, 3, Color.Black);
            bitmap.SetPixel(14, 5, Color.Black);
            bitmap.SetPixel(15, 1, Color.Black);
            bitmap.SetPixel(15, 4, Color.Black);
            return bitmap;
        }

        private Bitmap Create_R_Bitmap(Bitmap b) //add R to input image
        {
            //  +-----+
            //  |     |
            //  |    R|
            //  +-----+
            Bitmap bitmap = new Bitmap(b);
            bitmap.SetPixel(12, 11, Color.Black);
            bitmap.SetPixel(12, 12, Color.Black);
            bitmap.SetPixel(12, 13, Color.Black);
            bitmap.SetPixel(12, 14, Color.Black);
            bitmap.SetPixel(12, 15, Color.Black);
            bitmap.SetPixel(13, 11, Color.Black);
            bitmap.SetPixel(13, 13, Color.Black);
            bitmap.SetPixel(14, 11, Color.Black);
            bitmap.SetPixel(14, 13, Color.Black);
            bitmap.SetPixel(14, 14, Color.Black);
            bitmap.SetPixel(15, 12, Color.Black);
            bitmap.SetPixel(15, 15, Color.Black);
            return bitmap;
        }

        private Bitmap Create_O_Bitmap(Bitmap b) //add O to input image
        {
            //  +-----+
            //  |     |
            //  |    O|
            //  +-----+
            Bitmap bitmap = new Bitmap(b);
            bitmap.SetPixel(12, 12, Color.Black);
            bitmap.SetPixel(12, 13, Color.Black);
            bitmap.SetPixel(12, 14, Color.Black);
            bitmap.SetPixel(13, 11, Color.Black);
            bitmap.SetPixel(13, 15, Color.Black);
            bitmap.SetPixel(14, 11, Color.Black);
            bitmap.SetPixel(14, 15, Color.Black);
            bitmap.SetPixel(15, 12, Color.Black);
            bitmap.SetPixel(15, 13, Color.Black);
            bitmap.SetPixel(15, 14, Color.Black);
            return bitmap;
        }

        private Bitmap Create_P_Bitmap(Bitmap b) //add P to input image
        {
            //  +-----+
            //  |     |
            //  |    P|
            //  +-----+
            Bitmap bitmap = new Bitmap(b);
            bitmap.SetPixel(12, 11, Color.Black);
            bitmap.SetPixel(12, 12, Color.Black);
            bitmap.SetPixel(12, 13, Color.Black);
            bitmap.SetPixel(12, 14, Color.Black);
            bitmap.SetPixel(12, 15, Color.Black);
            bitmap.SetPixel(13, 11, Color.Black);
            bitmap.SetPixel(13, 13, Color.Black);
            bitmap.SetPixel(14, 11, Color.Black);
            bitmap.SetPixel(14, 13, Color.Black);
            bitmap.SetPixel(15, 12, Color.Black);
            return bitmap;
        }                  
    }
}
