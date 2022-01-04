using Microsoft.UI.Xaml;
using Microsoft.UI.Xaml.Controls;
using Microsoft.UI.Xaml.Controls.Primitives;
using Microsoft.UI.Xaml.Data;
using Microsoft.UI.Xaml.Input;
using Microsoft.UI.Xaml.Media;
using Microsoft.UI.Xaml.Navigation;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices.WindowsRuntime;
using Windows.Foundation;
using Windows.Foundation.Collections;

// To learn more about WinUI, the WinUI project structure,
// and more about our project templates, see: http://aka.ms/winui-project-info.

namespace Fonzy.UI
{
    /// <summary>
    /// An empty window that can be used on its own or navigated to within a Frame.
    /// </summary>
    public sealed partial class MainWindow : Window
    {
        public MainWindow()
        {
            this.InitializeComponent();
        }

        private void NavView_ItemInvoked(NavigationView sender, NavigationViewItemInvokedEventArgs args)
        {
            FrameNavigationOptions navOptions = new FrameNavigationOptions();
            navOptions.TransitionInfoOverride = args.RecommendedNavigationTransitionInfo;
            if ((string)args.InvokedItem == "Workspaces")
            {
                mainFrame.Navigate(typeof(Pages.WorkspacesPage));
            }
            else if ((string)args.InvokedItem == "Sorters")
            {
                mainFrame.Navigate(typeof(Pages.SortersPage));
            }
            else if ((string)args.InvokedItem == "PerfBins")
            {
                mainFrame.Navigate(typeof(Pages.PerfBinPage));
            }
            else if ((string)args.InvokedItem == "Shc")
            {
                mainFrame.Navigate(typeof(Pages.ShcPage));
            }
            else if ((string)args.InvokedItem == "Run")
            {
                mainFrame.Navigate(typeof(Pages.RunProcPage));
            }
        }
    }
}
